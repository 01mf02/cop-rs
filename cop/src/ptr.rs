use colosseum::unsync::Arena;
use core::hash::Hash;
use core::ops::Deref;
use hashbrown::HashSet;

/// Return equal pointers for any two equal dereferenceable objects.
///
/// ~~~
/// # use cop::ptr::normalise;
/// use colosseum::unsync::Arena;
/// use hashbrown::HashSet;
///
/// let s1 = "Hello".to_string();
/// let s2 = "Hello".to_string();
/// assert!(!(core::ptr::eq(&*s1, &*s2)));
///
/// let arena: Arena<String> = Arena::new();
/// let mut set: HashSet<&str> = HashSet::new();
/// let p1 = normalise(s1, &arena, &mut set);
/// let p2 = normalise(s2, &arena, &mut set);
/// assert!(core::ptr::eq(p1, p2));
/// ~~~
pub fn normalise<'a, T, Q>(x: T, arena: &'a Arena<T>, set: &mut HashSet<&'a Q>) -> &'a Q
where
    T: Deref<Target = Q>,
    Q: Eq + Hash + ?Sized,
{
    match set.get(&*x) {
        Some(y) => &y,
        None => {
            let y = arena.alloc(x);
            set.insert(y);
            y
        }
    }
}
