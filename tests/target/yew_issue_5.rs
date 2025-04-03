// Copyright (c) 2023 Tim Kurdov, licensed under MIT license
// Source: https://github.com/its-the-shrimp/yew-fmt
use yew::prelude::*;

#[function_component(Application)]
fn app() -> Html {
    let loading = false;
    yew::html! {
        if loading {
            { "Loading" }
        } else {
            { "Done" }
        }
    }
}
