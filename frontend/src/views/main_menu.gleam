import lustre/attribute
import lustre/element
import lustre/element/html
import lustre/event

import models.{AI, Human, NewGame, PlayerTypes}

pub fn view() -> element.Element(_) {
  html.div([attribute.class("stack")], [
    html.h1([], [element.text("CONNECT 4 LOL")]),
    html.button([event.on_click(NewGame(PlayerTypes(Human, Human)))], [
      element.text("Play locally"),
    ]),
    html.button([event.on_click(NewGame(PlayerTypes(Human, AI)))], [
      element.text("VS AI"),
    ]),
    html.button([event.on_click(NewGame(PlayerTypes(AI, AI)))], [
      element.text("AI VS AI"),
    ]),
  ])
}
