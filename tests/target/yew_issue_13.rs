// Copyright (c) 2023 Tim Kurdov, licensed under MIT license
// Source: https://github.com/its-the-shrimp/yew-fmt
use yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    yew::html! {
        <SomeComponent attr_one=true>
            { "Very long text that will stop these three lines from being merged" }
        </SomeComponent>
    }
}
