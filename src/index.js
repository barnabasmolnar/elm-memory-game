import { Elm } from "./Main.elm";

Elm.Main.init({
    node: document.getElementById("root"),
    flags: { isDev: process.env.NODE_ENV === "development" }
});