// Copyright (c) 2023 Tim Kurdov, licensed under MIT license, source https://github.com/its-the-shrimp/yew-fmt
use yew::prelude::*;

#[function_component(Application)]
fn app() -> Html {
    let loading = false;
    html! {
        if loading {
            { "Loading" }
        } else {
            { "Done" }
        }
    }
}
