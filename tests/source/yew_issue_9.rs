// Copyright (c) 2023 Tim Kurdov, licensed under MIT license, source https://github.com/its-the-shrimp/yew-fmt
use
yew::prelude::*;

#[function_component]
fn Comp() -> Html {
    yew::html! {
                        <Button
                            name="Play"
                            onclick={emitter.reform(|_| AppEvent::PreparePlay(None))}
                        ><img::Play/>
                        </Button>
    }
}
