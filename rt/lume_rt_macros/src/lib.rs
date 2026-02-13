mod link_section_data;

#[proc_macro_attribute]
pub fn link_section_data(attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    link_section_data::link_section_data(attr, item)
}
