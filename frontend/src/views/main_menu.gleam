import lustre/attribute
import lustre/element
import lustre/element/html
import lustre/event

import models.{AI, ChooseBot, Human, NewGame, PlayerTypes}
import shared.{type Turn, Red, Yellow}

// Model
pub type Model {
  Model(red: String, yellow: String)
}

pub fn new() {
  Model(red: "", yellow: "")
}

// Update
pub fn update(model: Model, turn: Turn, bot_name: String) {
  case turn {
    Red -> Model(..model, red: bot_name)
    Yellow -> Model(..model, yellow: bot_name)
  }
}

// View
pub fn view(model) -> element.Element(_) {
  html.div([], [home_page(), ai_match_config_pop_up(model)])
}

fn home_page() -> element.Element(_) {
  html.div([attribute.class("vstack")], [
    html.h1([], [element.text("CONNECT 4 LOL")]),
    html.button([event.on_click(NewGame(PlayerTypes(Human, Human)))], [
      element.text("Play locally"),
    ]),
    html.button([event.on_click(NewGame(PlayerTypes(Human, AI("minimax/3"))))], [
      element.text("VS AI"),
    ]),
    html.button(
      [event.on_click(NewGame(PlayerTypes(AI("move"), AI("minimax/5"))))],
      [element.text("AI VS AI")],
    ),
  ])
}

fn ai_match_config_pop_up(model: Model) -> element.Element(_) {
  html.div([attribute.class("vstack")], [
    html.div([attribute.class("hstack")], [
      player_setup(Red, model.red),
      player_setup(Yellow, model.yellow),
    ]),
    html.button(
      [
        attribute.disabled(model.red == "" || model.yellow == ""),
        event.on_click(NewGame(PlayerTypes(AI(model.red), AI(model.yellow)))),
      ],
      [element.text("start")],
    ),
  ])
}

fn player_setup(turn: Turn, selected: String) {
  html.form([attribute.class("flex-1")], [
    html.fieldset([attribute.class("vstack")], [
      html.legend([], [element.text("Choose a bot:")]),
      bot_radio_button("minimax/1", selected, turn),
      bot_radio_button("minimax/3", selected, turn),
      bot_radio_button("minimax/5", selected, turn),
    ]),
  ])
}

fn bot_radio_button(name: String, selected: String, turn: Turn) {
  echo name
  echo selected
  html.div([event.on_click(ChooseBot(turn, name))], [
    html.input([attribute.type_("radio"), attribute.checked(name == selected)]),
    html.label([], [element.text(name)]),
  ])
}
