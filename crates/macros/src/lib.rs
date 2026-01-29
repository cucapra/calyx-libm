use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{Data, DeriveInput, Fields, Ident, Index, parse_macro_input};

#[proc_macro_derive(Mangle)]
pub fn derive_mangle(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let (impl_generics, type_generics, where_clause) =
        input.generics.split_for_impl();

    let name = &input.ident;
    let buffer = format_ident!("w");

    let statements = mangle_data(&buffer, name, &input.data);

    let expanded = quote! {
        impl #impl_generics Mangle for #name #type_generics #where_clause {
            fn mangle(&self, #buffer: &mut dyn ::std::fmt::Write) -> ::std::fmt::Result {
                #statements
            }
        }
    };

    TokenStream::from(expanded)
}

/// Formats a `struct` or `enum` as an [`<expression>`].
///
/// [`<expression>`]:
///     https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangle.expression
fn mangle_data(buffer: &Ident, name: &Ident, data: &Data) -> TokenStream2 {
    let (code, statements) = match data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(fields) => {
                let calls = fields.named.iter().map(|field| {
                    let name = &field.ident;
                    let span = field.span();

                    quote_spanned! {span=>
                        self.#name.mangle(#buffer)
                    }
                });

                let statements = quote! { #( #calls?; )* };

                ("tl", statements)
            }
            Fields::Unnamed(fields) => {
                let calls =
                    fields.unnamed.iter().enumerate().map(|(i, field)| {
                        let index = Index::from(i);
                        let span = field.span();

                        quote_spanned! {span=>
                            self.#index.mangle(#buffer)
                        }
                    });

                let statements = quote! { #( #calls?; )* };

                ("tl", statements)
            }
            _ => unimplemented!(),
        },
        Data::Enum(_) => {
            let statements = quote! {
                write!(#buffer, "{}", *self as usize)?;
            };

            ("L", statements)
        }
        _ => unimplemented!(),
    };

    let identifier = name.to_string();
    let prefix = format!("{}{}{}", code, identifier.len(), identifier);

    quote! {
        #buffer.write_str(#prefix)?;
        #statements
        #buffer.write_char('E')
    }
}
