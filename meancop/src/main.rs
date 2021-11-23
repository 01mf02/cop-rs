use clap::Clap;
use colosseum::unsync::Arena;
use cop::{fof, lean, nano, szs};
use cop::{Args, Clause, Fof, Lit, LitMat, Offset, Signed};
use log::info;
use meancop::{cli, parse, preprocess, Error};
use std::fs::File;
use std::io::Write;

/// Automated theorem prover for first-order logic with equality
///
/// Set the environment variable "LOG" to "info", "debug", or "trace"
/// to obtain an increasingly detailed log.
#[derive(Clap)]
struct Cli {
    /// Enable conjecture-directed proof search
    #[clap(long)]
    conj: bool,

    /// Disable matrix sorting by number of paths
    #[clap(long)]
    nopaths: bool,

    #[clap(long, short)]
    nonclausal: bool,

    /// Order of context processing in nonclausal search
    ///
    /// Possible values are: in, inout, and out (default).
    #[clap(long, default_value = "out")]
    ctx_order: CtxOrder,

    #[clap(flatten)]
    deepening: cli::Deepening,

    #[clap(flatten)]
    cut: cli::Cut,

    #[clap(flatten)]
    paths: cli::Paths,
}

#[derive(Clap, PartialEq, Eq)]
enum CtxOrder {
    In,
    InOut,
    Out,
}

impl core::str::FromStr for CtxOrder {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, String> {
        match s {
            "in" => Ok(Self::In),
            "inout" => Ok(Self::InOut),
            "out" => Ok(Self::Out),
            _ => Err("unrecognised context order".to_string()),
        }
    }
}

fn main() {
    use env_logger::Env;
    // log warnings and errors by default, do not print timestamps
    env_logger::from_env(Env::default().filter_or("LOG", "warn"))
        .format_timestamp(None)
        .init();

    let cli = Cli::parse();
    let arena: Arena<String> = Arena::new();

    let result = run(&cli, &arena);
    if let Err(e) = result {
        print!("{}", szs::Status(e.get_kind()));
        if let Some(e) = e.get_error() {
            cli.paths.output(e).unwrap()
        };
        std::process::exit(1);
    }
}

fn run(cli: &Cli, arena: &Arena<String>) -> Result<(), Error> {
    let fm = if let Some(fm) = parse::parse(&cli.paths.file)?.join() {
        fm
    } else {
        print!("{}", szs::Status(szs::Satisfiable));
        return Ok(());
    };
    info!("joined: {}", fm);

    let fm = preprocess::add_eq_axioms(fm)?;
    info!("equalised: {}", fm);

    // "#" marks clauses stemming from the conjecture
    // we can interpret it as "$true"
    let (hashed, fm) = if cli.conj {
        fm.mark_impl(|| Fof::atom("#".to_string(), Args::new()))
    } else {
        (false, fm)
    };
    info!("hashed: {}", fm);

    let fm = fm.map_atoms(&mut |a| a.to_lit(|| "=".to_string()).map_head(Signed::from));

    let fm = if cli.nonclausal {
        fm.qnnf(&Fof::unfold_eqfm_conj_impl)
    } else {
        fm.qnnf(&Fof::unfold_eqfm_disj_conj)
    };
    info!("unfolded: {}", fm);

    let fm = -fm;
    info!("nnf: {}", fm);
    let fm = fm.fresh_vars(&mut Default::default(), &mut 0);
    info!("fresh vars: {}", fm);
    let fm = fm.skolem_outer(&mut fof::SkolemState::new(("skolem".to_string(), 0)));
    info!("skolemised: {}", fm);

    let fm = if cli.nopaths { fm } else { fm.order().0 };
    info!("ordered: {}", fm);

    let mut set = Default::default();
    let fm = fm.map_literals(&mut |l| l.symbolise(&mut set, arena));
    let hash = Lit::new(Signed::from("#".to_string()), Args::new()).symbolise(&mut set, arena);

    if cli.nonclausal {
        let matrix = nano::Matrix::from(fm);
        log::info!("matrix: {}", matrix);

        let matrix = matrix
            .into_iter()
            .map(|cl| {
                let mut map = Default::default();
                let mut st = 0;
                cl.fresh_vars(&mut map, &mut st)
            })
            .collect();

        search_nonclausal(matrix, hashed.then(|| hash), cli)
    } else {
        let fm = fm.cnf();
        info!("cnf: {}", fm);

        let mut matrix = preprocess::matrix(fm);
        info!("matrix: {}", matrix);

        if !hashed {
            preprocess::hash_matrix(&mut matrix, &hash)
        }
        info!("hashed: {}", matrix);
        search_clausal(matrix, hash, cli)
    }
}

use preprocess::SLit;

fn search_nonclausal(
    matrix: nano::Matrix<SLit, usize>,
    hash: Option<SLit>,
    cli: &Cli,
) -> Result<(), Error> {
    let mut pre_cps: Vec<_> = matrix.pre_cps().collect();
    for cp in &mut pre_cps {
        for ctx in &mut cp.ctx {
            if cli.ctx_order == CtxOrder::In {
                ctx.move_beta_left()
            }
            if cli.ctx_order == CtxOrder::Out {
                ctx.move_beta_right()
            }
        }
    }
    let db: cop::Db<_, _> = pre_cps.into_iter().map(|cp| cp.db_entry()).collect();
    info!("db: {}", db);

    let make_start = || {
        let hash = hash.as_ref().cloned().map(LitMat::Lit);
        hash.unwrap_or_else(|| LitMat::Mat(matrix.positive()))
    };
    let start = Clause::from([make_start()]);
    let mut off = start.bound_vars().max().map(|v| v + 1).unwrap_or(0);

    let mut infs = Vec::new();
    let cuts = cli.cut.get_cuts();
    for lim in cli.deepening.depths() {
        use cop::nano::search::{Opt, Search};
        info!("search with depth {}", lim);

        let opt = Opt { cuts, lim };
        let mut search = Search::new(&start, &db, opt);
        let proof = search.prove().cloned();
        let inf = search.inferences();
        info!("depth {} completed after {} inferences", lim, inf);
        infs.push(inf);

        if let Some(steps) = proof {
            let infs_sum: usize = infs.iter().sum();
            info!("proof found after {} inferences", infs_sum);

            let mut steps = steps.iter().cloned();
            let proof = cop::nano::Proof::from_iter(&mut steps, &mut off);
            assert!(steps.next().is_none());

            if let Some(file) = &cli.paths.stats {
                let mut f = File::create(file)?;
                let infs = serde_json::to_string(&infs).unwrap();
                writeln!(f, r#"{{ "infs": {} }}"#, infs)?;
            };

            let start = make_start();
            let start = Offset::new(0, &start);
            assert!(proof.check(&search.sub, start, Default::default()));
            print!("{}", szs::Status(szs::Theorem));
            let proof = proof.display(start);
            cli.paths.output(proof)?;
            return Ok(());
        }
    }

    Err(Error::from(szs::Incomplete))
}

fn search_clausal(matrix: lean::Matrix<SLit>, hash: SLit, cli: &Cli) -> Result<(), Error> {
    let db = matrix.contrapositives().map(|cp| cp.db_entry()).collect();
    info!("db: {}", db);
    let start = Clause::from([&hash]);

    let mut infs = Vec::new();
    let cuts = cli.cut.get_cuts();
    for lim in cli.deepening.depths() {
        use cop::lean::search::{Opt, Search};
        info!("search with depth {}", lim);
        let opt = Opt { cuts, lim };
        let mut search = Search::new(&start, &db, opt);
        let proof = search.prove().cloned();
        let inf = search.inferences();
        info!("depth {} completed after {} inferences", lim, inf);
        infs.push(inf);

        if let Some(steps) = proof {
            let infs_sum: usize = infs.iter().sum();
            info!("proof found after {} inferences", infs_sum);

            let proof = cop::lean::Proof::from_iter(&mut steps.iter().cloned(), &mut 0);

            if let Some(file) = &cli.paths.stats {
                let mut f = File::create(file)?;
                let infs = serde_json::to_string(&infs).unwrap();
                writeln!(f, r#"{{ "infs": {} }}"#, infs)?;
            };

            let hash = Offset::new(0, &hash);
            assert!(proof.check(&search.sub, hash, Default::default()));
            print!("{}", szs::Status(szs::Theorem));
            let proof = proof.display(hash);
            cli.paths.output(proof)?;
            return Ok(());
        }
    }

    Err(Error::from(szs::Incomplete))
}
