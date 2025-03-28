import gleam/int

import lustre/attribute
import lustre/element
import lustre/element/html
import lustre/event

import models.{
  AI, Human, NewGame, PlayerTypes, UserEnteredNumberOfGames, UserSelectedBot,
  UserStartedAIBattle,
}
import shared.{type Turn, Red, Yellow}

// Model
pub type Model {
  Model(red: String, yellow: String, number_of_games: String)
}

pub fn new() {
  Model(red: "", yellow: "", number_of_games: "")
}

fn is_number_of_games_valid(model: Model) {
  case int.parse(model.number_of_games) {
    Ok(parsed) -> {
      0 < parsed && parsed <= 30
    }
    Error(_) -> False
  }
}

fn is_form_valid(model: Model) {
  model.red != "" && model.yellow != "" && is_number_of_games_valid(model)
}

// Update
pub fn update_selected_bot(model: Model, turn: Turn, bot_name: String) -> Model {
  case turn {
    Red -> Model(..model, red: bot_name)
    Yellow -> Model(..model, yellow: bot_name)
  }
}

pub fn update_number_of_games(model: Model, number_of_games: String) {
  Model(..model, number_of_games:)
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
    html.button([event.on_click(NewGame(PlayerTypes(Human, AI("minimax/5"))))], [
      element.text("VS AI"),
    ]),
    html.button(
      [event.on_click(NewGame(PlayerTypes(AI("minimax/2"), AI("minimax/5"))))],
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
    number_of_games_field(model),
    html.button(
      [
        attribute.disabled(!is_form_valid(model)),
        event.on_click(UserStartedAIBattle(
          model.red,
          model.yellow,
          model.number_of_games,
        )),
      ],
      [element.text("start")],
    ),
  ])
}

fn player_setup(turn: Turn, selected: String) {
  html.form([attribute.class("flex-1")], [
    html.fieldset([attribute.class("vstack")], [
      html.legend([], [element.text("Select a bot:")]),
      bot_radio_button("minimax/1", selected, turn),
      bot_radio_button("minimax/3", selected, turn),
      bot_radio_button("minimax/5", selected, turn),
      bot_radio_button("minimax/6", selected, turn),
    ]),
  ])
}

fn bot_radio_button(
  name: String,
  selected: String,
  turn: Turn,
) -> element.Element(_) {
  echo name
  echo selected
  html.div([event.on_click(UserSelectedBot(turn, name))], [
    html.input([attribute.type_("radio"), attribute.checked(name == selected)]),
    html.label([], [element.text(name)]),
  ])
}

fn number_of_games_field(model: Model) -> element.Element(_) {
  html.div([attribute.class("hstack")], [
    html.label([], [element.text("Number of games: ")]),
    html.input([
      attribute.class("flex-1"),
      attribute.placeholder("Enter number of games to run"),
      attribute.value(model.number_of_games),
      attribute.type_("number"),
      event.on_input(UserEnteredNumberOfGames),
    ]),
  ])
}
