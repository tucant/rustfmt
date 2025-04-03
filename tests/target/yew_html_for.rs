// Copyright (c) 2023 Tim Kurdov, licensed under MIT license
// Source: https://github.com/its-the-shrimp/yew-fmt
// config: yew.html_flavor="Ext"
use yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    yew::html! {
        for i in 0 .. 10 {
            <code>{ i }</code>
            <br />
        }
    }
}
