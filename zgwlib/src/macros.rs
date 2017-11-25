#[macro_export]
macro_rules! hash_map(
    { $($key:expr => $value:expr),+ } => {
        {
            let mut m = ::std::collections::HashMap::new();
            $(
                m.insert($key, $value);
            )+
            m
        }
    };
);

#[macro_export]
macro_rules! hash_set(
    { $($value:expr),+ } => {
        {
            let mut m = ::std::collections::HashSet::new();
            $(
                m.insert($value);
            )+
            m
        }
    };
);

#[macro_export]
macro_rules! vec_deque(
    { $($value:expr),+ } => {
        {
            let mut m = ::std::collections::VecDeque::new();
            $(
                m.push_back($value);
            )+
            m
        }
    };
);