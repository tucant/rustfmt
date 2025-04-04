// Copyright (c) 2023 Tim Kurdov, licensed under MIT license
// Source: https://github.com/its-the-shrimp/yew-fmt
use
yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    yew::html!(<div id={


        let id = "idk"; id} />);
    yew::html!(<div id={let id = "one line
                             another line"; id} />)
}
