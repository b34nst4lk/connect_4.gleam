-module(lustre@element@svg).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([animate/1, animate_motion/1, animate_transform/1, mpath/1, set/1, circle/1, ellipse/1, line/1, polygon/1, polyline/1, rect/1, a/2, defs/2, g/2, marker/2, mask/2, missing_glyph/2, pattern/2, svg/2, switch/2, symbol/2, desc/2, metadata/2, title/2, fe_blend/1, fe_color_matrix/1, fe_component_transfer/1, fe_composite/1, fe_convolve_matrix/1, fe_diffuse_lighting/2, fe_displacement_map/1, fe_drop_shadow/1, fe_flood/1, fe_func_a/1, fe_func_b/1, fe_func_g/1, fe_func_r/1, fe_gaussian_blur/1, fe_image/1, fe_merge/2, fe_merge_node/1, fe_morphology/1, fe_offset/1, fe_specular_lighting/2, fe_tile/2, fe_turbulence/1, linear_gradient/2, radial_gradient/2, stop/1, image/1, path/1, text/2, use_/1, fe_distant_light/1, fe_point_light/1, fe_spot_light/1, clip_path/2, script/2, style/2, foreign_object/2, text_path/2, tspan/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/lustre/element/svg.gleam", 19).
?DOC("\n").
-spec animate(list(lustre@internals@vdom:attribute(QDK))) -> lustre@internals@vdom:element(QDK).
animate(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animate"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 24).
?DOC("\n").
-spec animate_motion(list(lustre@internals@vdom:attribute(QDO))) -> lustre@internals@vdom:element(QDO).
animate_motion(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animateMotion"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 29).
?DOC("\n").
-spec animate_transform(list(lustre@internals@vdom:attribute(QDS))) -> lustre@internals@vdom:element(QDS).
animate_transform(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animateTransform"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 34).
?DOC("\n").
-spec mpath(list(lustre@internals@vdom:attribute(QDW))) -> lustre@internals@vdom:element(QDW).
mpath(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"mpath"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 39).
?DOC("\n").
-spec set(list(lustre@internals@vdom:attribute(QEA))) -> lustre@internals@vdom:element(QEA).
set(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"set"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 46).
?DOC("\n").
-spec circle(list(lustre@internals@vdom:attribute(QEE))) -> lustre@internals@vdom:element(QEE).
circle(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"circle"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 51).
?DOC("\n").
-spec ellipse(list(lustre@internals@vdom:attribute(QEI))) -> lustre@internals@vdom:element(QEI).
ellipse(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"ellipse"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 56).
?DOC("\n").
-spec line(list(lustre@internals@vdom:attribute(QEM))) -> lustre@internals@vdom:element(QEM).
line(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"line"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 61).
?DOC("\n").
-spec polygon(list(lustre@internals@vdom:attribute(QEQ))) -> lustre@internals@vdom:element(QEQ).
polygon(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"polygon"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 66).
?DOC("\n").
-spec polyline(list(lustre@internals@vdom:attribute(QEU))) -> lustre@internals@vdom:element(QEU).
polyline(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"polyline"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 71).
?DOC("\n").
-spec rect(list(lustre@internals@vdom:attribute(QEY))) -> lustre@internals@vdom:element(QEY).
rect(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"rect"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 78).
?DOC("\n").
-spec a(
    list(lustre@internals@vdom:attribute(QFC)),
    list(lustre@internals@vdom:element(QFC))
) -> lustre@internals@vdom:element(QFC).
a(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"a"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 86).
?DOC("\n").
-spec defs(
    list(lustre@internals@vdom:attribute(QFI)),
    list(lustre@internals@vdom:element(QFI))
) -> lustre@internals@vdom:element(QFI).
defs(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"defs"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 94).
?DOC("\n").
-spec g(
    list(lustre@internals@vdom:attribute(QFO)),
    list(lustre@internals@vdom:element(QFO))
) -> lustre@internals@vdom:element(QFO).
g(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"g"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 102).
?DOC("\n").
-spec marker(
    list(lustre@internals@vdom:attribute(QFU)),
    list(lustre@internals@vdom:element(QFU))
) -> lustre@internals@vdom:element(QFU).
marker(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"marker"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 110).
?DOC("\n").
-spec mask(
    list(lustre@internals@vdom:attribute(QGA)),
    list(lustre@internals@vdom:element(QGA))
) -> lustre@internals@vdom:element(QGA).
mask(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"mask"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 118).
?DOC("\n").
-spec missing_glyph(
    list(lustre@internals@vdom:attribute(QGG)),
    list(lustre@internals@vdom:element(QGG))
) -> lustre@internals@vdom:element(QGG).
missing_glyph(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"missing-glyph"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 126).
?DOC("\n").
-spec pattern(
    list(lustre@internals@vdom:attribute(QGM)),
    list(lustre@internals@vdom:element(QGM))
) -> lustre@internals@vdom:element(QGM).
pattern(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"pattern"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 134).
?DOC("\n").
-spec svg(
    list(lustre@internals@vdom:attribute(QGS)),
    list(lustre@internals@vdom:element(QGS))
) -> lustre@internals@vdom:element(QGS).
svg(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"svg"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 142).
?DOC("\n").
-spec switch(
    list(lustre@internals@vdom:attribute(QGY)),
    list(lustre@internals@vdom:element(QGY))
) -> lustre@internals@vdom:element(QGY).
switch(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"switch"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 150).
?DOC("\n").
-spec symbol(
    list(lustre@internals@vdom:attribute(QHE)),
    list(lustre@internals@vdom:element(QHE))
) -> lustre@internals@vdom:element(QHE).
symbol(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"symbol"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 160).
?DOC("\n").
-spec desc(
    list(lustre@internals@vdom:attribute(QHK)),
    list(lustre@internals@vdom:element(QHK))
) -> lustre@internals@vdom:element(QHK).
desc(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"desc"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 168).
?DOC("\n").
-spec metadata(
    list(lustre@internals@vdom:attribute(QHQ)),
    list(lustre@internals@vdom:element(QHQ))
) -> lustre@internals@vdom:element(QHQ).
metadata(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"metadata"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 176).
?DOC("\n").
-spec title(
    list(lustre@internals@vdom:attribute(QHW)),
    list(lustre@internals@vdom:element(QHW))
) -> lustre@internals@vdom:element(QHW).
title(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"title"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 186).
?DOC("\n").
-spec fe_blend(list(lustre@internals@vdom:attribute(QIC))) -> lustre@internals@vdom:element(QIC).
fe_blend(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feBlend"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 191).
?DOC("\n").
-spec fe_color_matrix(list(lustre@internals@vdom:attribute(QIG))) -> lustre@internals@vdom:element(QIG).
fe_color_matrix(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feColorMatrix"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 196).
?DOC("\n").
-spec fe_component_transfer(list(lustre@internals@vdom:attribute(QIK))) -> lustre@internals@vdom:element(QIK).
fe_component_transfer(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feComponentTransfer"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 201).
?DOC("\n").
-spec fe_composite(list(lustre@internals@vdom:attribute(QIO))) -> lustre@internals@vdom:element(QIO).
fe_composite(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feComposite"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 206).
?DOC("\n").
-spec fe_convolve_matrix(list(lustre@internals@vdom:attribute(QIS))) -> lustre@internals@vdom:element(QIS).
fe_convolve_matrix(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feConvolveMatrix"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 211).
?DOC("\n").
-spec fe_diffuse_lighting(
    list(lustre@internals@vdom:attribute(QIW)),
    list(lustre@internals@vdom:element(QIW))
) -> lustre@internals@vdom:element(QIW).
fe_diffuse_lighting(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDiffuseLighting"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 219).
?DOC("\n").
-spec fe_displacement_map(list(lustre@internals@vdom:attribute(QJC))) -> lustre@internals@vdom:element(QJC).
fe_displacement_map(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDisplacementMap"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 224).
?DOC("\n").
-spec fe_drop_shadow(list(lustre@internals@vdom:attribute(QJG))) -> lustre@internals@vdom:element(QJG).
fe_drop_shadow(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDropShadow"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 229).
?DOC("\n").
-spec fe_flood(list(lustre@internals@vdom:attribute(QJK))) -> lustre@internals@vdom:element(QJK).
fe_flood(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFlood"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 234).
?DOC("\n").
-spec fe_func_a(list(lustre@internals@vdom:attribute(QJO))) -> lustre@internals@vdom:element(QJO).
fe_func_a(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncA"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 239).
?DOC("\n").
-spec fe_func_b(list(lustre@internals@vdom:attribute(QJS))) -> lustre@internals@vdom:element(QJS).
fe_func_b(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncB"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 244).
?DOC("\n").
-spec fe_func_g(list(lustre@internals@vdom:attribute(QJW))) -> lustre@internals@vdom:element(QJW).
fe_func_g(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncG"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 249).
?DOC("\n").
-spec fe_func_r(list(lustre@internals@vdom:attribute(QKA))) -> lustre@internals@vdom:element(QKA).
fe_func_r(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncR"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 254).
?DOC("\n").
-spec fe_gaussian_blur(list(lustre@internals@vdom:attribute(QKE))) -> lustre@internals@vdom:element(QKE).
fe_gaussian_blur(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feGaussianBlur"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 259).
?DOC("\n").
-spec fe_image(list(lustre@internals@vdom:attribute(QKI))) -> lustre@internals@vdom:element(QKI).
fe_image(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feImage"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 264).
?DOC("\n").
-spec fe_merge(
    list(lustre@internals@vdom:attribute(QKM)),
    list(lustre@internals@vdom:element(QKM))
) -> lustre@internals@vdom:element(QKM).
fe_merge(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMerge"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 272).
?DOC("\n").
-spec fe_merge_node(list(lustre@internals@vdom:attribute(QKS))) -> lustre@internals@vdom:element(QKS).
fe_merge_node(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMergeNode"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 277).
?DOC("\n").
-spec fe_morphology(list(lustre@internals@vdom:attribute(QKW))) -> lustre@internals@vdom:element(QKW).
fe_morphology(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMorphology"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 282).
?DOC("\n").
-spec fe_offset(list(lustre@internals@vdom:attribute(QLA))) -> lustre@internals@vdom:element(QLA).
fe_offset(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feOffset"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 287).
?DOC("\n").
-spec fe_specular_lighting(
    list(lustre@internals@vdom:attribute(QLE)),
    list(lustre@internals@vdom:element(QLE))
) -> lustre@internals@vdom:element(QLE).
fe_specular_lighting(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feSpecularLighting"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 295).
?DOC("\n").
-spec fe_tile(
    list(lustre@internals@vdom:attribute(QLK)),
    list(lustre@internals@vdom:element(QLK))
) -> lustre@internals@vdom:element(QLK).
fe_tile(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feTile"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 303).
?DOC("\n").
-spec fe_turbulence(list(lustre@internals@vdom:attribute(QLQ))) -> lustre@internals@vdom:element(QLQ).
fe_turbulence(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feTurbulence"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 310).
?DOC("\n").
-spec linear_gradient(
    list(lustre@internals@vdom:attribute(QLU)),
    list(lustre@internals@vdom:element(QLU))
) -> lustre@internals@vdom:element(QLU).
linear_gradient(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"linearGradient"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 318).
?DOC("\n").
-spec radial_gradient(
    list(lustre@internals@vdom:attribute(QMA)),
    list(lustre@internals@vdom:element(QMA))
) -> lustre@internals@vdom:element(QMA).
radial_gradient(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"radialGradient"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 326).
?DOC("\n").
-spec stop(list(lustre@internals@vdom:attribute(QMG))) -> lustre@internals@vdom:element(QMG).
stop(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"stop"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 333).
?DOC("\n").
-spec image(list(lustre@internals@vdom:attribute(QMK))) -> lustre@internals@vdom:element(QMK).
image(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"image"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 338).
?DOC("\n").
-spec path(list(lustre@internals@vdom:attribute(QMO))) -> lustre@internals@vdom:element(QMO).
path(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"path"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 343).
?DOC("\n").
-spec text(list(lustre@internals@vdom:attribute(QMS)), binary()) -> lustre@internals@vdom:element(QMS).
text(Attrs, Content) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"text"/utf8>>,
        Attrs,
        [lustre@element:text(Content)]
    ).

-file("src/lustre/element/svg.gleam", 348).
?DOC("\n").
-spec use_(list(lustre@internals@vdom:attribute(QMW))) -> lustre@internals@vdom:element(QMW).
use_(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"use"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 355).
?DOC("\n").
-spec fe_distant_light(list(lustre@internals@vdom:attribute(QNA))) -> lustre@internals@vdom:element(QNA).
fe_distant_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDistantLight"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 360).
?DOC("\n").
-spec fe_point_light(list(lustre@internals@vdom:attribute(QNE))) -> lustre@internals@vdom:element(QNE).
fe_point_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"fePointLight"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 365).
?DOC("\n").
-spec fe_spot_light(list(lustre@internals@vdom:attribute(QNI))) -> lustre@internals@vdom:element(QNI).
fe_spot_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feSpotLight"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 372).
?DOC("\n").
-spec clip_path(
    list(lustre@internals@vdom:attribute(QNM)),
    list(lustre@internals@vdom:element(QNM))
) -> lustre@internals@vdom:element(QNM).
clip_path(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"clipPath"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 380).
?DOC("\n").
-spec script(list(lustre@internals@vdom:attribute(QNS)), binary()) -> lustre@internals@vdom:element(QNS).
script(Attrs, Js) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"script"/utf8>>,
        Attrs,
        [lustre@element:text(Js)]
    ).

-file("src/lustre/element/svg.gleam", 385).
?DOC("\n").
-spec style(list(lustre@internals@vdom:attribute(QNW)), binary()) -> lustre@internals@vdom:element(QNW).
style(Attrs, Css) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"style"/utf8>>,
        Attrs,
        [lustre@element:text(Css)]
    ).

-file("src/lustre/element/svg.gleam", 392).
?DOC("\n").
-spec foreign_object(
    list(lustre@internals@vdom:attribute(QOA)),
    list(lustre@internals@vdom:element(QOA))
) -> lustre@internals@vdom:element(QOA).
foreign_object(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"foreignObject"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 400).
?DOC("\n").
-spec text_path(
    list(lustre@internals@vdom:attribute(QOG)),
    list(lustre@internals@vdom:element(QOG))
) -> lustre@internals@vdom:element(QOG).
text_path(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"textPath"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 408).
?DOC("\n").
-spec tspan(
    list(lustre@internals@vdom:attribute(QOM)),
    list(lustre@internals@vdom:element(QOM))
) -> lustre@internals@vdom:element(QOM).
tspan(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"tspan"/utf8>>,
        Attrs,
        Children
    ).
