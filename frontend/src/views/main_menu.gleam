import lustre/element
import lustre/element/html
import lustre/event

import messages as msg

pub fn view() -> element.Element(_) {
  html.div([], [
    html.h1([], [
      element.text("CONNECT 4 LOL"),
      html.button([event.on_click(msg.NewGame)], [element.text("NEW GAME")]),
    ]),
  ])
}
