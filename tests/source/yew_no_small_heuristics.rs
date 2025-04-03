// Copyright (c) 2023 Tim Kurdov, licensed under MIT license, source https://github.com/its-the-shrimp/yew-fmt
// config: yew.use_small_heuristics="Off",yew.html_flavor="Ext"

use
yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    yew::html! {
<>
<div>
<code>
{ "Код!" }
</code>
</div>
if true { { "true" } } else {{"false"}}
for i in 0 .. 10 {{i}}
</>
    }
}
