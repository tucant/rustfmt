// Copyright (c) 2023 Tim Kurdov, licensed under MIT license
// Source: https://github.com/its-the-shrimp/yew-fmt
use yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    yew::html! {
        <ul style="display: flex; flex-wrap: wrap; gap: 1.5rem; padding: 0; margin: 0; list-style: none; border: 1px solid #ccc">
            <strong>{"Hello, World"}</strong>
        </ul>
    }
}
