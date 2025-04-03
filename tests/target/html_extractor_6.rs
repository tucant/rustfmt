// rustfmt-format_strings: true
pub fn main() {
    html_extractor::html! {
        <div id="inhalt">
            "verylongsttringthatshouldbesplitupbecauseitissolongverylongsttringthatshouldbesplitupbecauseitissolongverylongsttringthatshouldbesplitupbecauseitissolong"
            <div style="padding:0px; width:650px; margin:0px; background-color:#ffffff;">
                <p>
                    <strong>
                        <a href="https://www.example.org/reallyreallylongverylongandlonglongveryreallylongurlthatshouldbebrokenupmaybe" target="_blank">
                            "Hier"
                        </a>
    };
}
