// Copyright (c) 2023 Tim Kurdov, licensed under MIT license, source https://github.com/its-the-shrimp/yew-fmt
use yew::prelude::*;

#[function_component(Application)]
fn app() -> Html { 
let names = vec!["Sam","Bob","Ray"];

yew::html! {
<div id="introductions">
{
names.into_iter().map(|name| {
yew::html!{<div key={name}>{ format!("Hello, I'am {}!",name) }</div>}
}).collect::<Html>()
}
</div>
};
}