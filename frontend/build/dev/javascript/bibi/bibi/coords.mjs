import { CustomType as $CustomType } from "../gleam.mjs";

export class Coords extends $CustomType {
  constructor(x, y) {
    super();
    this.x = x;
    this.y = y;
  }
}
