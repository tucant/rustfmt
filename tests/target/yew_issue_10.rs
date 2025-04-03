// Copyright (c) 2023 Tim Kurdov, licensed under MIT license, source https://github.com/its-the-shrimp/yew-fmt
use yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    html! {
        <>
            <Smth ..props />
            <Smth ..props>{ "Something else" }</Smth>
        </>
    }
}
