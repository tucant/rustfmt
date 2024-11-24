// rustfmt-format_strings: true
pub fn main() {
    // the string is so long it prevents reformatting
    html_extractor::html! {
        <ul class="nav depth_2 linkItemContainer">
            <li class="tree depth_2 linkItem branchLinkItem " title="Archiv" id=_linkclass>
                <a class=_linkclass href={&format!(
                    "/scripts/mgrqispi.dll?APPNAME=CampusNet&PRGNAME=EXTERNALPAGES&\
                     ARGUMENTS=-N{id:015},-N000464,-Avvarchivstart%2Ehtml"
                )}>
    };
}
