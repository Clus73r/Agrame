
use gloo_console::log;
use yew::prelude::*;
use web_sys::{InputEvent};

#[function_component]
pub fn TextInput() -> Html {
    // let (state, dispatch) = use_store::<GrammarState>();
    // let onchange: Callback<Event> = dispatch.reduce_mut_callback(|state| state.te += 1);
    log!("textinput run");
    let oninput = Callback::from(move |_event: InputEvent| {
        crate::update();
        // update_callback.emit(false);
        // let text_area_element = event.target_dyn_into::<HtmlTextAreaElement>();
        // let pos_grammar = agrame::grammar_parse::parse(&text_area_element.expect("Grammar text input").value());
        // match pos_grammar {
        //     Ok(grammar) => {
        //         dispatch.set(GrammarState{
        //             grammar,
        //         })
        //     },
        //     Err(_) => log!("Grammar could not be constructed."),
        // };
    });
    html! {
        <textarea {oninput} id="text_input">{state.te}</textarea>
    }
}
