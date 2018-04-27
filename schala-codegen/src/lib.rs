#![feature(proc_macro)]
extern crate proc_macro;
use proc_macro::TokenStream;

#[proc_macro]
pub fn print_a_thing(_input: TokenStream) -> TokenStream {
  "println!(\"Invoked from a proc macro\");".parse().unwrap()
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
