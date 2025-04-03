// Copyright (c) 2023 Tim Kurdov, licensed under MIT license
// Source: https://github.com/its-the-shrimp/yew-fmt
// config: yew.html_flavor="Ext"
use yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    yew::html! {
        <>
            <div>
                let Some(x) = 4u8.checked_add(1) else { return };
                <p>{ x }</p>
                <p>{ x * 2 }</p>
            </div>
            if let Some(x) = 69u8.checked_add(420) {
                let y = todo!();
                <p>{ x + y }</p>
            }
        </>
    }
}
