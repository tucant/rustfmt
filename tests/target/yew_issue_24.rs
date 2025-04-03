// Copyright (c) 2023 Tim Kurdov, licensed under MIT license
// Source: https://github.com/its-the-shrimp/yew-fmt
use yew::prelude::*;

#[function_component]
pub fn MyComponent() -> Html {
    yew::html! { <Select<<Type as Weight>::Unit> class={classes!("color-red-500")} {on_change} /> }
}
