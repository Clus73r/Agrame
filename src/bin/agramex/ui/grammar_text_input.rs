use gloo_console::log;
use yew::prelude::*;
use web_sys::{InputEvent, HtmlTextAreaElement};
use yewdux::use_store;

use crate::GrammarState;

#[function_component]
pub fn GrammarTextInput() -> Html {
    // let (state, dispatch) = use_store::<GrammarState>();
    // let onchange: Callback<Event> = dispatch.reduce_mut_callback(|state| state.te += 1);
    log!("Grammarinput run");
    let oninput = Callback::from(move |_: InputEvent| {
        crate::update();
        // let text_area_element = event.target_dyn_into::<HtmlTextAreaElement>();
        // let text_input = event.target_dyn_into::<yew::web
        // let pos_grammar = agrame::grammar_parse::parse(&text_area_element.expect("Grammar text input").value());
        // match pos_grammar {
        //     Ok(grammar) => log!(format!("{}", grammar)),
        //     Err(_) => log!("Grammar could not be constructed."),
        // };
    });
    html! {
        <textarea {oninput} id="grammar_input">{state.te}</textarea>
    }
}


