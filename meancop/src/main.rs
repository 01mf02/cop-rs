use clap::Clap;
use colosseum::unsync::Arena;
use cop::lean::{Clause, Proof};
use cop::Offset;
use cop::{fof, szs};
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

    #[clap(flatten)]
    preprocess: preprocess::Options,

    #[clap(flatten)]
    deepening: cli::Deepening,

    #[clap(flatten)]
    cut: cli::Cut,

    #[clap(flatten)]
    paths: cli::Paths,
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

    use preprocess::hash_fof;

    let (hashed, fm) = if cli.conj { hash_fof(fm) } else { (false, fm) };
    info!("hashed: {}", fm);

    let (mut matrix, hash) = preprocess::preprocess(fm, &cli.preprocess, arena);

    if !hashed {
        preprocess::hash_matrix(&mut matrix, &hash)
    }
    info!("hashed: {}", matrix);

    let db = matrix.contrapositives().collect();
    info!("db: {}", db);
    let start = Clause::from(fof::Dnf::Lit(hash.clone()));
    let start = Offset::new(0, &start);

    let mut infs = Vec::new();
    let cuts = cli.cut.get_cuts();
    for lim in cli.deepening.depths() {
        use cop::lean::search::{Opt, Search, Task};
        info!("search with depth {}", lim);
        let opt = Opt { cuts, lim };
        let mut search = Search::new(Task::new(start), &db, opt);
        let proof = search.prove().cloned();
        let inf = search.inferences();
        info!("depth {} completed after {} inferences", lim, inf);
        infs.push(inf);

        if let Some(steps) = proof {
            let infs_sum: usize = infs.iter().sum();
            info!("proof found after {} inferences", infs_sum);

            let proof = Proof::from_iter(&mut steps.iter().cloned(), &mut 0);

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
