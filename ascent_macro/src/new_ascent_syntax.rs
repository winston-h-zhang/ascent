#![deny(warnings)]
extern crate proc_macro;
use proc_macro2::{Span, TokenStream};
use syn::parse::{Parse, ParseStream};
use syn::{
    braced, parenthesized, punctuated::Punctuated, spanned::Spanned, Attribute, Error,
    Expr, Generics, Ident, Pat, Result, Token, Type, Visibility,
};

use derive_syn_parse::Parse;
use itertools::Itertools;
use quote::ToTokens;

use crate::utils::pat_to_ident;

// resources:
// https://blog.rust-lang.org/2018/12/21/Procedural-Macros-in-Rust-2018.html
// https://github.com/dtolnay/syn/blob/master/examples/lazy-static/lazy-static/src/lib.rs
// https://crates.io/crates/quote
// example: https://gitlab.gnome.org/federico/gnome-class/-/blob/master/src/parser/mod.rs

mod kw {
    syn::custom_keyword!(relation);
    syn::custom_keyword!(lattice);
    syn::custom_punctuation!(LongLeftArrow, <--);
    syn::custom_keyword!(agg);
    syn::custom_keyword!(ident);
    syn::custom_keyword!(expr);
}

#[derive(Clone, Debug)]
pub(crate) struct Signatures {
    pub(crate) declaration: TypeSignature,
    pub(crate) implementation: Option<ImplSignature>,
}

// impl Signatures {
//     pub fn split_ty_generics_for_impl(
//         &self,
//     ) -> (ImplGenerics<'_>, TypeGenerics<'_>, Option<&'_ WhereClause>) {
//         self.declaration.generics.split_for_impl()
//     }

//     pub fn split_impl_generics_for_impl(
//         &self,
//     ) -> (ImplGenerics<'_>, TypeGenerics<'_>, Option<&'_ WhereClause>) {
//         let Some(signature) = &self.implementation else {
//             return self.split_ty_generics_for_impl();
//         };

//         let (impl_generics, _, _) = signature.impl_generics.split_for_impl();
//         let (_, ty_generics, where_clause) = signature.generics.split_for_impl();

//         (impl_generics, ty_generics, where_clause)
//     }
// }

impl Parse for Signatures {
    fn parse(input: ParseStream) -> Result<Self> {
        let declaration = TypeSignature::parse(input)?;
        let implementation = if input.peek(Token![impl]) {
            Some(ImplSignature::parse(input)?)
        } else {
            None
        };
        Ok(Signatures {
            declaration,
            implementation,
        })
    }
}

impl ToTokens for Signatures {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let declaration = &self.declaration;
        let implementation = &self.implementation;

        tokens.extend(quote! {
            #declaration
            #implementation
        });
    }
}

#[derive(Clone, Parse, Debug)]
pub struct TypeSignature {
    // We don't actually use the Parse impl to parse attrs.
    #[call(Attribute::parse_outer)]
    pub attrs: Vec<Attribute>,
    pub visibility: Visibility,
    pub _struct_kw: Token![struct],
    pub ident: Ident,
    #[call(parse_generics_with_where_clause)]
    pub generics: Generics,
    pub _semi: Token![;],
}

impl ToTokens for TypeSignature {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let attrs = &self.attrs;
        let vis = &self.visibility;
        let ident = &self.ident;
        let generics = &self.generics;

        tokens.extend(quote! {
            #(#attrs)*
            #vis struct #ident #generics;
        });
    }
}

#[derive(Clone, Parse, Debug)]
pub struct ImplSignature {
    pub _impl_kw: Token![impl],
    pub impl_generics: Generics,
    pub ident: Ident,
    #[call(parse_generics_with_where_clause)]
    pub generics: Generics,
    pub _semi: Token![;],
}

impl ToTokens for ImplSignature {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let impl_generics = &self.impl_generics;
        let ident = &self.ident;
        let generics = &self.generics;

        tokens.extend(quote! {
            impl #impl_generics #ident #generics;
        });
    }
}

/// Parse impl on Generics does not parse WhereClauses, hence this function
fn parse_generics_with_where_clause(input: ParseStream) -> Result<Generics> {
    let mut res = Generics::parse(input)?;
    if input.peek(Token![where]) {
        res.where_clause = Some(input.parse()?);
    }
    Ok(res)
}

// #[derive(Clone)]
pub struct RelationNode {
    pub attrs: Vec<Attribute>,
    pub name: Ident,
    pub field_types: Punctuated<Type, Token![,]>,
    pub initialization: Option<Expr>,
    pub _semi_colon: Token![;],
    pub is_lattice: bool,
}

impl Parse for RelationNode {
    fn parse(input: ParseStream) -> Result<Self> {
        let is_lattice = input.peek(kw::lattice);
        if is_lattice {
            input.parse::<kw::lattice>()?;
        } else {
            input.parse::<kw::relation>()?;
        }
        let name: Ident = input.parse()?;
        let content;
        parenthesized!(content in input);
        let field_types = content.parse_terminated(Type::parse, Token![,])?;
        let initialization = if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            Some(input.parse::<Expr>()?)
        } else {
            None
        };

        let _semi_colon = input.parse::<Token![;]>()?;
        if is_lattice && field_types.empty_or_trailing() {
            return Err(input.error("empty lattice is not allowed"));
        }
        Ok(RelationNode {
            attrs: vec![],
            name,
            field_types,
            _semi_colon,
            is_lattice,
            initialization,
        })
    }
}

impl ToTokens for RelationNode {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = &self.name;
        let field_types = &self.field_types;
        let attrs = &self.attrs;
        let kw = if self.is_lattice {
            quote!(lattice)
        } else {
            quote!(relation)
        };

        tokens.extend(quote! {
            #(#attrs)*
            #kw #name(#field_types);
        });
    }
}

#[derive(Parse, Clone)]
pub enum BodyItemNode {
    #[peek(Token![for], name = "generative clause")]
    Generator(GeneratorNode),
    #[peek(kw::agg, name = "aggregate clause")]
    Agg(AggClauseNode),
    #[peek_with(peek_macro_invocation, name = "macro invocation")]
    MacroInvocation(syn::ExprMacro),
    #[peek(Ident, name = "body clause")]
    Clause(BodyClauseNode),
    #[peek(Token![!], name = "negation clause")]
    Negation(NegationClauseNode),
    #[peek(syn::token::Paren, name = "disjunction node")]
    Disjunction(DisjunctionNode),
    #[peek_with(peek_if_or_let, name = "if condition or let binding")]
    Cond(CondClause),
}

fn peek_macro_invocation(parse_stream: ParseStream) -> bool {
    parse_stream.peek(Ident) && parse_stream.peek2(Token![!])
}

fn peek_if_or_let(parse_stream: ParseStream) -> bool {
    parse_stream.peek(Token![if]) || parse_stream.peek(Token![let])
}

impl ToTokens for BodyItemNode {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            BodyItemNode::Clause(clause) => {
                let rel = &clause.rel;
                let args = &clause.args;
                tokens.extend(quote! { #rel(#args) });
            }
            BodyItemNode::Generator(gen) => {
                let pattern = &gen.pattern;
                let expr = &gen.expr;
                tokens.extend(quote! { for #pattern in #expr });
            }
            BodyItemNode::Cond(cond) => cond.to_tokens(tokens),
            BodyItemNode::Agg(agg) => {
                let pat = &agg.pat;
                let aggregator = &agg.aggregator;
                let bound_args = &agg.bound_args;
                let rel = &agg.rel;
                let rel_args = &agg.rel_args;
                tokens.extend(quote! { agg #pat = #aggregator(#bound_args) in #rel(#rel_args) });
            }
            BodyItemNode::Negation(neg) => {
                let rel = &neg.rel;
                let args = &neg.args;
                tokens.extend(quote! { !#rel(#args) });
            }
            BodyItemNode::Disjunction(disj) => disj.to_tokens(tokens),
            BodyItemNode::MacroInvocation(mac) => mac.to_tokens(tokens),
        }
    }
}

#[derive(Clone)]
pub struct DisjunctionNode {
    _paren: syn::token::Paren,
    disjuncts: Punctuated<Punctuated<BodyItemNode, Token![,]>, Token![||]>,
}

impl Parse for DisjunctionNode {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        let _paren = parenthesized!(content in input);
        let res: Punctuated<Punctuated<BodyItemNode, Token![,]>, Token![||]> =
            Punctuated::<Punctuated<BodyItemNode, Token![,]>, Token![||]>::parse_terminated_with(
                &content,
                Punctuated::<BodyItemNode, Token![,]>::parse_separated_nonempty,
            )?;
        Ok(DisjunctionNode {
            _paren,
            disjuncts: res,
        })
    }
}

impl ToTokens for DisjunctionNode {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let disjuncts = self.disjuncts.iter().map(|conj| {
            quote! { #conj }
        });
        tokens.extend(quote! { (#(#disjuncts)||*) });
    }
}

#[derive(Parse, Clone)]
pub struct GeneratorNode {
    pub for_keyword: Token![for],
    #[call(Pat::parse_multi)]
    pub pattern: Pat,
    pub _in_keyword: Token![in],
    pub expr: Expr,
}

#[derive(Clone)]
pub struct BodyClauseNode {
    pub rel: Ident,
    pub args: Punctuated<BodyClauseArg, Token![,]>,
    pub cond_clauses: Vec<CondClause>,
}

#[derive(Parse, Clone, PartialEq, Eq, Debug)]
pub enum BodyClauseArg {
    #[peek(Token![?], name = "Pattern arg")]
    Pat(ClauseArgPattern),
    #[peek_with({ |_| true }, name = "Expression arg")]
    Expr(Expr),
}

// impl BodyClauseArg {
//     pub fn unwrap_expr(self) -> Expr {
//         match self {
//             Self::Expr(exp) => exp,
//             Self::Pat(_) => panic!("unwrap_expr(): BodyClauseArg is not an expr"),
//         }
//     }

//     pub fn unwrap_expr_ref(&self) -> &Expr {
//         match self {
//             Self::Expr(exp) => exp,
//             Self::Pat(_) => panic!("unwrap_expr(): BodyClauseArg is not an expr"),
//         }
//     }

//     pub fn get_vars(&self) -> Vec<Ident> {
//         match self {
//             BodyClauseArg::Pat(p) => pattern_get_vars(&p.pattern),
//             BodyClauseArg::Expr(e) => expr_to_ident(e).into_iter().collect(),
//         }
//     }
// }

impl ToTokens for BodyClauseArg {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            BodyClauseArg::Pat(pat) => {
                pat.huh_token.to_tokens(tokens);
                pat.pattern.to_tokens(tokens);
            }
            BodyClauseArg::Expr(exp) => exp.to_tokens(tokens),
        }
    }
}

#[derive(Parse, Clone, PartialEq, Eq, Debug)]
pub struct ClauseArgPattern {
    pub huh_token: Token![?],
    #[call(Pat::parse_multi)]
    pub pattern: Pat,
}

#[derive(Parse, Clone, PartialEq, Eq, Hash, Debug)]
pub struct IfLetClause {
    pub if_keyword: Token![if],
    pub let_keyword: Token![let],
    #[call(Pat::parse_multi)]
    pub pattern: Pat,
    pub eq_symbol: Token![=],
    pub exp: syn::Expr,
}

#[derive(Parse, Clone, PartialEq, Eq, Hash, Debug)]
pub struct IfClause {
    pub if_keyword: Token![if],
    pub cond: Expr,
}

#[derive(Parse, Clone, PartialEq, Eq, Hash, Debug)]
pub struct LetClause {
    pub let_keyword: Token![let],
    #[call(Pat::parse_multi)]
    pub pattern: Pat,
    pub eq_symbol: Token![=],
    pub exp: syn::Expr,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum CondClause {
    IfLet(IfLetClause),
    If(IfClause),
    Let(LetClause),
}

// impl CondClause {
//     pub fn bound_vars(&self) -> Vec<Ident> {
//         match self {
//             CondClause::IfLet(cl) => pattern_get_vars(&cl.pattern),
//             CondClause::If(_) => vec![],
//             CondClause::Let(cl) => pattern_get_vars(&cl.pattern),
//         }
//     }

//     /// returns the expression associated with the CondClause.
//     /// Useful for determining clause dependencies
//     pub fn expr(&self) -> &Expr {
//         match self {
//             CondClause::IfLet(cl) => &cl.exp,
//             CondClause::If(cl) => &cl.cond,
//             CondClause::Let(cl) => &cl.exp,
//         }
//     }
// }

impl Parse for CondClause {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Token![if]) {
            if input.peek2(Token![let]) {
                let cl: IfLetClause = input.parse()?;
                Ok(Self::IfLet(cl))
            } else {
                let cl: IfClause = input.parse()?;
                Ok(Self::If(cl))
            }
        } else if input.peek(Token![let]) {
            let cl: LetClause = input.parse()?;
            Ok(Self::Let(cl))
        } else {
            Err(input.error("expected either if clause or if let clause"))
        }
    }
}

impl ToTokens for CondClause {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            CondClause::If(if_clause) => {
                let cond = &if_clause.cond;
                tokens.extend(quote! { if #cond });
            }
            CondClause::IfLet(if_let_clause) => {
                let pattern = &if_let_clause.pattern;
                let expr = &if_let_clause.exp;
                tokens.extend(quote! { if let #pattern = #expr });
            }
            CondClause::Let(let_clause) => {
                let pattern = &let_clause.pattern;
                let expr = &let_clause.exp;
                tokens.extend(quote! { let #pattern = #expr });
            }
        }
    }
}

impl Parse for BodyClauseNode {
    fn parse(input: ParseStream) -> Result<Self> {
        let rel: Ident = input.parse()?;
        let args_content;
        parenthesized!(args_content in input);
        let args = args_content.parse_terminated(BodyClauseArg::parse, Token![,])?;
        let mut cond_clauses = vec![];
        while let Ok(cl) = input.parse() {
            cond_clauses.push(cl);
        }
        Ok(BodyClauseNode {
            rel,
            args,
            cond_clauses,
        })
    }
}

#[derive(Parse, Clone)]
pub struct NegationClauseNode {
    _neg_token: Token![!],
    pub rel: Ident,
    #[paren]
    _rel_arg_paren: syn::token::Paren,
    #[inside(_rel_arg_paren)]
    #[call(Punctuated::parse_terminated)]
    pub args: Punctuated<Expr, Token![,]>,
}

#[derive(Clone, Parse)]
pub enum HeadItemNode {
    #[peek_with(peek_macro_invocation, name = "macro invocation")]
    MacroInvocation(syn::ExprMacro),
    #[peek(Ident, name = "head clause")]
    HeadClause(HeadClauseNode),
}

impl HeadItemNode {
    pub fn clause(&self) -> &HeadClauseNode {
        match self {
            HeadItemNode::HeadClause(cl) => cl,
            HeadItemNode::MacroInvocation(_) => panic!("unexpected macro invocation"),
        }
    }
}

impl ToTokens for HeadItemNode {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            HeadItemNode::HeadClause(clause) => clause.to_tokens(tokens),
            HeadItemNode::MacroInvocation(mac) => mac.to_tokens(tokens),
        }
    }
}

#[derive(Clone)]
pub struct HeadClauseNode {
    pub rel: Ident,
    pub args: Punctuated<Expr, Token![,]>,
}

impl Parse for HeadClauseNode {
    fn parse(input: ParseStream) -> Result<Self> {
        let rel: Ident = input.parse()?;
        let args_content;
        parenthesized!(args_content in input);
        let args = args_content.parse_terminated(Expr::parse, Token![,])?;
        Ok(HeadClauseNode { rel, args })
    }
}

impl ToTokens for HeadClauseNode {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let rel = &self.rel;
        let args = &self.args;
        tokens.extend(quote! { #rel(#args) });
    }
}

#[derive(Clone, Parse)]
pub struct AggClauseNode {
    pub agg_kw: kw::agg,
    #[call(Pat::parse_multi)]
    pub pat: Pat,
    pub _eq_token: Token![=],
    pub aggregator: AggregatorNode,
    #[paren]
    pub _agg_arg_paren: syn::token::Paren,
    #[inside(_agg_arg_paren)]
    #[call(Punctuated::parse_terminated)]
    pub bound_args: Punctuated<Ident, Token![,]>,
    pub _in_kw: Token![in],
    pub rel: Ident,
    #[paren]
    _rel_arg_paren: syn::token::Paren,
    #[inside(_rel_arg_paren)]
    #[call(Punctuated::parse_terminated)]
    pub rel_args: Punctuated<Expr, Token![,]>,
}

#[derive(Clone)]
pub enum AggregatorNode {
    Path(syn::Path),
    Expr(Expr),
}

impl Parse for AggregatorNode {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(syn::token::Paren) {
            let inside_parens;
            parenthesized!(inside_parens in input);
            Ok(AggregatorNode::Expr(inside_parens.parse()?))
        } else {
            Ok(AggregatorNode::Path(input.parse()?))
        }
    }
}

// impl AggregatorNode {
//     pub fn get_expr(&self) -> Expr {
//         match self {
//             AggregatorNode::Path(path) => parse2(quote! {#path}).unwrap(),
//             AggregatorNode::Expr(expr) => expr.clone(),
//         }
//     }
// }

impl ToTokens for AggregatorNode {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            AggregatorNode::Path(path) => path.to_tokens(tokens),
            AggregatorNode::Expr(expr) => expr.to_tokens(tokens),
        }
    }
}

pub struct RuleNode {
    pub attrs: Vec<Attribute>,
    pub head_clauses: Vec<HeadItemNode>, // Punctuated<HeadItemNode, Token![,]>,
    pub body_items: Vec<BodyItemNode>,   // Punctuated<BodyItemNode, Token![,]>,
}

impl Parse for RuleNode {
    fn parse(input: ParseStream) -> Result<Self> {
        let head_clauses = if input.peek(syn::token::Brace) {
            let content;
            braced!(content in input);
            Punctuated::<HeadItemNode, Token![,]>::parse_terminated(&content)?
        } else {
            Punctuated::<HeadItemNode, Token![,]>::parse_separated_nonempty(input)?
        };
        let head_clauses = head_clauses.into_iter().collect();

        if input.peek(Token![;]) {
            // println!("fact rule!!!");
            input.parse::<Token![;]>()?;
            Ok(RuleNode {
                attrs: vec![],
                head_clauses,
                body_items: vec![], /*Punctuated::default()*/
            })
        } else {
            input.parse::<Token![<]>()?;
            input.parse::<Token![-]>()?;
            input.parse::<Token![-]>()?;
            // NOTE this does not work with quote!
            // input.parse::<kw::LongLeftArrow>()?;

            let body_items =
                Punctuated::<BodyItemNode, Token![,]>::parse_separated_nonempty(input)?;
            input.parse::<Token![;]>()?;
            Ok(RuleNode {
                attrs: vec![],
                head_clauses,
                body_items: body_items.into_iter().collect(),
            })
        }
    }
}

impl ToTokens for RuleNode {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        // let attrs = &self.attrs;
        let head_clauses = &self.head_clauses;
        let body_items = &self.body_items;

        tokens.extend(quote! {
            #(#head_clauses),* <-- #(#body_items),*;
        });
    }
}

// TODO maybe remove?
#[allow(dead_code)]
pub(crate) fn rule_node_summary(rule: &RuleNode) -> String {
    fn bitem_to_str(bitem: &BodyItemNode) -> String {
        match bitem {
            BodyItemNode::Generator(gen) => {
                format!(
                    "for_{}",
                    pat_to_ident(&gen.pattern)
                        .map(|x| x.to_string())
                        .unwrap_or_default()
                )
            }
            BodyItemNode::Clause(bcl) => format!("{}", bcl.rel),
            BodyItemNode::Disjunction(_) => todo!(),
            BodyItemNode::Cond(_cl) => format!("if_"),
            BodyItemNode::Agg(agg) => format!("agg {}", agg.rel),
            BodyItemNode::Negation(neg) => format!("! {}", neg.rel),
            BodyItemNode::MacroInvocation(m) => format!("{:?}!(..)", m.mac.path),
        }
    }
    fn hitem_to_str(hitem: &HeadItemNode) -> String {
        match hitem {
            HeadItemNode::MacroInvocation(m) => format!("{:?}!(..)", m.mac.path),
            HeadItemNode::HeadClause(cl) => cl.rel.to_string(),
        }
    }
    format!(
        "{} <-- {}",
        rule.head_clauses.iter().map(hitem_to_str).join(", "),
        rule.body_items.iter().map(bitem_to_str).join(", ")
    )
}

#[derive(Parse)]
pub struct MacroDefParam {
    _dollar: Token![$],
    name: Ident,
    _colon: Token![:],
    kind: MacroParamKind,
}

impl ToTokens for MacroDefParam {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = &self.name;
        let kind = &self.kind;

        tokens.extend(quote! {
            $#name:#kind
        });
    }
}

#[derive(Parse)]
#[allow(unused)]
pub enum MacroParamKind {
    #[peek(kw::ident, name = "ident")]
    Expr(Ident),
    #[peek(kw::expr, name = "expr")]
    Ident(Ident),
}

impl ToTokens for MacroParamKind {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            MacroParamKind::Expr(ident) => ident.to_tokens(tokens),
            MacroParamKind::Ident(ident) => ident.to_tokens(tokens),
        }
    }
}

#[derive(Parse)]
pub struct MacroDefNode {
    _mac: Token![macro],
    name: Ident,
    #[paren]
    _arg_paren: syn::token::Paren,
    #[inside(_arg_paren)]
    #[call(Punctuated::parse_terminated)]
    params: Punctuated<MacroDefParam, Token![,]>,
    #[brace]
    _body_brace: syn::token::Brace,
    #[inside(_body_brace)]
    body: TokenStream,
}

impl ToTokens for MacroDefNode {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = &self.name;
        let params = &self.params;
        let body = &self.body;

        tokens.extend(quote! {
            macro #name(#params) {
                #body
            }
        });
    }
}

// #[derive(Clone)]
pub(crate) struct NewAscentProgram {
    pub rules: Vec<RuleNode>,
    pub relations: Vec<RelationNode>,
    pub signatures: Option<Signatures>,
    pub attributes: Vec<syn::Attribute>,
    pub macros: Vec<MacroDefNode>,
}

impl Parse for NewAscentProgram {
    fn parse(input: ParseStream) -> Result<Self> {
        let attributes = Attribute::parse_inner(input)?;
        let mut struct_attrs = Attribute::parse_outer(input)?;
        let signatures = if input.peek(Token![pub]) || input.peek(Token![struct]) {
            let mut signatures = Signatures::parse(input)?;
            signatures.declaration.attrs = std::mem::take(&mut struct_attrs);
            Some(signatures)
        } else {
            None
        };
        let mut rules = vec![];
        let mut relations = vec![];
        let mut macros = vec![];
        while !input.is_empty() {
            let attrs = if !struct_attrs.is_empty() {
                std::mem::take(&mut struct_attrs)
            } else {
                Attribute::parse_outer(input)?
            };
            if input.peek(kw::relation) || input.peek(kw::lattice) {
                let mut relation_node = RelationNode::parse(input)?;
                relation_node.attrs = attrs;
                relations.push(relation_node);
            } else if input.peek(Token![macro]) {
                if !attrs.is_empty() {
                    return Err(Error::new(attrs[0].span(), "unexpected attribute(s)"));
                }
                macros.push(MacroDefNode::parse(input)?);
            } else {
                let mut rule_node = RuleNode::parse(input)?;
                rule_node.attrs = attrs;
                rules.push(rule_node);
            }
        }
        Ok(NewAscentProgram {
            rules,
            relations,
            signatures,
            attributes,
            macros,
        })
    }
}

impl ToTokens for NewAscentProgram {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let relations = &self.relations;
        let rules = &self.rules;
        let attributes = &self.attributes;
        let signatures = &self.signatures;
        let macros = &self.macros;

        tokens.extend(quote! {
            ascent! {
                #(#attributes)*
                #signatures
                #(#macros)*
                #(#relations)*
                #(#rules)*
            }
        });
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub(crate) struct RelationIdentity {
    pub name: Ident,
    pub field_types: Vec<Type>,
    pub is_lattice: bool,
}

impl From<&RelationNode> for RelationIdentity {
    fn from(relation_node: &RelationNode) -> Self {
        RelationIdentity {
            name: relation_node.name.clone(),
            field_types: relation_node.field_types.iter().cloned().collect(),
            is_lattice: relation_node.is_lattice,
        }
    }
}

// #[derive(Clone)]
// pub(crate) struct DsAttributeContents {
//     pub path: syn::Path,
//     pub args: TokenStream,
// }

// impl Parse for DsAttributeContents {
//     fn parse(input: ParseStream) -> Result<Self> {
//         let content = input;
//         // parenthesized!(content in input);

//         let path = syn::Path::parse_mod_style(content)?;
//         let args = if content.peek(Token![:]) {
//             content.parse::<Token![:]>()?;
//             TokenStream::parse(content)?
//         } else {
//             TokenStream::default()
//         };

//         Ok(Self { path, args })
//     }
// }

fn rule_desugar_with_binding(rule: &mut RuleNode, relations: &[RelationNode]) -> Result<Option<RelationNode>> {
    let mut bindings = Vec::new();
        let mut binding_tys = Vec::new();

        // Parse the with_bindings attribute
        let binding_relation_name = rule.attrs.iter().find_map(|attr| {
            if attr.path().is_ident("with_bindings") {
                attr.parse_args::<Ident>().ok()
            } else {
                None
            }
        });

        for body_item in &rule.body_items {
            if let BodyItemNode::Clause(clause) = body_item {
                let relation = relations
                    .iter()
                    .find(|r| r.name == clause.rel)
                    .unwrap();

                for (i, arg) in clause.args.iter().enumerate() {
                    if let BodyClauseArg::Expr(Expr::Path(expr_path)) = arg {
                        if let Some(ident) = expr_path.path.get_ident() {
                            bindings.push(ident.clone());
                            binding_tys.push(relation.field_types[i].clone());
                        }
                    }
                }
            }
        }

        if !bindings.is_empty() {

            let binding_relation_name = binding_relation_name.unwrap_or_else(|| {
                let join_clauses = rule.head_clauses.iter().map(|hi| &hi.clause().rel).join("_");
                Ident::new(&format!("{}_bindings", join_clauses), Span::call_site())
            });

            let binding_relation = RelationNode {
                attrs: Vec::new(),
                name: binding_relation_name.clone(),
                field_types: Punctuated::from_iter(binding_tys.iter().cloned()),
                initialization: None,
                _semi_colon: Token![;](Span::call_site()),
                is_lattice: false,
            };
            
            let binding_head_clause = HeadClauseNode {
                rel: binding_relation_name,
                args: Punctuated::from_iter(bindings.iter().map(|ident| {
                    Expr::Path(syn::ExprPath {
                        attrs: Vec::new(),
                        qself: None,
                        path: syn::Path::from(ident.clone()),
                    })
                })),
            };
            
            rule.head_clauses.push(HeadItemNode::HeadClause(binding_head_clause));
            Ok(Some(binding_relation))
        } else {
            Ok(None)
        }
}

fn rules_desugar_with_binding(prog: &mut NewAscentProgram) -> Result<()> {
    let mut new_relations = Vec::new();

    for rule in &mut prog.rules {
        if let Some(new_relation) = rule_desugar_with_binding(rule, &prog.relations)? {
            new_relations.push(new_relation);
        }
    }

    prog.relations.extend(new_relations);
    Ok(())
}

pub(crate) fn compile_new_ascent_to_ascent(mut new_prog: NewAscentProgram) -> Result<TokenStream> {
    rules_desugar_with_binding(&mut new_prog)?;

    let mut output = TokenStream::new();
    new_prog.to_tokens(&mut output);
    print!("HELLO IS THIS EVEN POSSIBLE");
    Ok(output)

}
