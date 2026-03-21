const LUME_GRAMMAR: &str = include_str!("../../lume.ungram");

fn main() {
    grammar::generate(LUME_GRAMMAR);
}
