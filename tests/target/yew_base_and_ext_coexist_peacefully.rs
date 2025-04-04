// Copyright (c) 2023 Tim Kurdov, licensed under MIT license
// Source: https://github.com/its-the-shrimp/yew-fmt
// the following is an yew::html! invocation that's valid in base Yew syntax, but invalid in
// yew-html-ext
use yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    yew::html! {
        match x {
            42 => <Foo as Bar>::idk(),
            _ => yew::html!(),
        }
    }
}
