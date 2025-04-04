// Copyright (c) 2023 Tim Kurdov, licensed under MIT license
// Source: https://github.com/its-the-shrimp/yew-fmt
// config: hard_tabs=true

impl Component for MyComponent {
	fn view(&self, _ctx: &Context<Self>) -> Html {
		yew::html! {
			{ match num {
				1 => yew::html! {
					{"1"}
				},
				_ => yew::html! {
					{"???"}
				}
			} }
		}
	}
}
