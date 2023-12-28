use gloo_console::log;
use yew::prelude::*;
use web_sys::{InputEvent, HtmlTextAreaElement};

#[function_component]
pub fn GrammarTextInput() -> Html {
    let oninput = Callback::from(move |event: InputEvent| {
        let text_area_element = event.target_dyn_into::<HtmlTextAreaElement>();
        // let text_input = event.target_dyn_into::<yew::web
        let pos_grammar = agrame::grammar_parse::parse(&text_area_element.expect("Grammar text input").value());
        match pos_grammar {
            Ok(grammar) => log!(format!("{}", grammar)),
            Err(_) => log!("Grammar could not be constructed."),
        };
    });
    html! {
        <textarea {oninput} id="text_display"></textarea>
    }
}


