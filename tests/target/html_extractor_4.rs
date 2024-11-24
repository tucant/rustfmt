// rustfmt-format_strings: true
pub fn main() {
    // the string is so long it prevents reformatting
    html_extractor::html! {
        <a href={&format!(
            "/scripts/mgrqispi.dll?APPNAME=CampusNet&PRGNAME=EXTERNALPAGES&ARGUMENTS=-N{id:015},\
             -N000464,-Avvarchivstart%2Ehtml"
        )}>
            "Archiv"
        </a>
    };
}
