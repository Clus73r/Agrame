use yew::prelude::*;


#[function_component]
fn App() -> Html {
    let counter = use_state(|| 0);
    let onclick = {
        let counter = counter.clone();
        move |_| {
            let value = *counter + 1;
            counter.set(value);
        }
    };

    html! {
        <div>
            <button {onclick}>{"+1"}</button>
            <p>{*counter}</p>
            <TextDisplay />
        </div>
    }
}

#[function_component]
fn TextDisplay() -> Html {
    html! {
        <textarea id="text_display"></textarea>
    }
}

fn main() {
    yew::Renderer::<App>::new().render();
}
