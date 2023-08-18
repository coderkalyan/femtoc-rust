extern crate proc_macro;

use quote::quote;
use syn::{parse_macro_input, DeriveInput, Data, Fields};

#[proc_macro_derive(ExtraData)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let expanded = match input.data {
        Data::Struct(ref data) => {
            match data.fields {
                Fields::Named(ref fields) => {
                    let named0 = fields.named.pairs().map(|x| x.value().ident.clone());
                    let named1 = named0.clone();
                    let types = fields.named.pairs().map(|x| x.value().ty.clone());

                    let indices = (0..(named0.len() as u32)).collect::<Vec<_>>();

                    quote! {
                        impl extra::ExtraData<#name> for #name {
                            fn pack(&self, vec: &mut extra::Vec) {
                                #(vec.push(self.#named0.into());)*
                            }

                            fn unpack(slice: extra::Slice, index: extra::Index) -> #name {
                                #name {
                                    #(#named1: #types::from(slice[index + #indices].value() as usize)),*
                                }
                            }
                        }
                    }
                },
                Fields::Unnamed(ref _fields) => {
                    quote!()
                },
                Fields::Unit => {
                    quote!()
                }
            }
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    };

    // panic!("{:?}", expanded);
    proc_macro::TokenStream::from(expanded)

    // let DeriveInput { ident, data, .. } = parse_macro_input!(input);
    // let output = quote! {
    //     impl Pack for #ident {}
    // };
    // output.into()
}

