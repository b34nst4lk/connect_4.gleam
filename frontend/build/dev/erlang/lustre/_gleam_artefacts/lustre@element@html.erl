-module(lustre@element@html).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([html/2, text/1, base/1, head/2, link/1, meta/1, style/2, title/2, body/2, address/2, article/2, aside/2, footer/2, header/2, h1/2, h2/2, h3/2, h4/2, h5/2, h6/2, hgroup/2, main/2, nav/2, section/2, search/2, blockquote/2, dd/2, 'div'/2, dl/2, dt/2, figcaption/2, figure/2, hr/1, li/2, menu/2, ol/2, p/2, pre/2, ul/2, a/2, abbr/2, b/2, bdi/2, bdo/2, br/1, cite/2, code/2, data/2, dfn/2, em/2, i/2, kbd/2, mark/2, q/2, rp/2, rt/2, ruby/2, s/2, samp/2, small/2, span/2, strong/2, sub/2, sup/2, time/2, u/2, var/2, wbr/1, area/1, audio/2, img/1, map/2, track/1, video/2, embed/1, iframe/1, object/1, picture/2, portal/1, source/1, svg/2, math/2, canvas/1, noscript/2, script/2, del/2, ins/2, caption/2, col/1, colgroup/2, table/2, tbody/2, td/2, tfoot/2, th/2, thead/2, tr/2, button/2, datalist/2, fieldset/2, form/2, input/1, label/2, legend/2, meter/2, optgroup/2, option/2, output/2, progress/2, select/2, textarea/2, details/2, dialog/2, summary/2, slot/1, template/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/lustre/element/html.gleam", 9).
?DOC("\n").
-spec html(
    list(lustre@internals@vdom:attribute(OXR)),
    list(lustre@internals@vdom:element(OXR))
) -> lustre@internals@vdom:element(OXR).
html(Attrs, Children) ->
    lustre@element:element(<<"html"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 16).
-spec text(binary()) -> lustre@internals@vdom:element(any()).
text(Content) ->
    lustre@element:text(Content).

-file("src/lustre/element/html.gleam", 23).
?DOC("\n").
-spec base(list(lustre@internals@vdom:attribute(OXZ))) -> lustre@internals@vdom:element(OXZ).
base(Attrs) ->
    lustre@element:element(<<"base"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 28).
?DOC("\n").
-spec head(
    list(lustre@internals@vdom:attribute(OYD)),
    list(lustre@internals@vdom:element(OYD))
) -> lustre@internals@vdom:element(OYD).
head(Attrs, Children) ->
    lustre@element:element(<<"head"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 36).
?DOC("\n").
-spec link(list(lustre@internals@vdom:attribute(OYJ))) -> lustre@internals@vdom:element(OYJ).
link(Attrs) ->
    lustre@element:element(<<"link"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 41).
?DOC("\n").
-spec meta(list(lustre@internals@vdom:attribute(OYN))) -> lustre@internals@vdom:element(OYN).
meta(Attrs) ->
    lustre@element:element(<<"meta"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 46).
?DOC("\n").
-spec style(list(lustre@internals@vdom:attribute(OYR)), binary()) -> lustre@internals@vdom:element(OYR).
style(Attrs, Css) ->
    lustre@element:element(<<"style"/utf8>>, Attrs, [text(Css)]).

-file("src/lustre/element/html.gleam", 51).
?DOC("\n").
-spec title(list(lustre@internals@vdom:attribute(OYV)), binary()) -> lustre@internals@vdom:element(OYV).
title(Attrs, Content) ->
    lustre@element:element(<<"title"/utf8>>, Attrs, [text(Content)]).

-file("src/lustre/element/html.gleam", 58).
?DOC("\n").
-spec body(
    list(lustre@internals@vdom:attribute(OYZ)),
    list(lustre@internals@vdom:element(OYZ))
) -> lustre@internals@vdom:element(OYZ).
body(Attrs, Children) ->
    lustre@element:element(<<"body"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 68).
?DOC("\n").
-spec address(
    list(lustre@internals@vdom:attribute(OZF)),
    list(lustre@internals@vdom:element(OZF))
) -> lustre@internals@vdom:element(OZF).
address(Attrs, Children) ->
    lustre@element:element(<<"address"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 76).
?DOC("\n").
-spec article(
    list(lustre@internals@vdom:attribute(OZL)),
    list(lustre@internals@vdom:element(OZL))
) -> lustre@internals@vdom:element(OZL).
article(Attrs, Children) ->
    lustre@element:element(<<"article"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 84).
?DOC("\n").
-spec aside(
    list(lustre@internals@vdom:attribute(OZR)),
    list(lustre@internals@vdom:element(OZR))
) -> lustre@internals@vdom:element(OZR).
aside(Attrs, Children) ->
    lustre@element:element(<<"aside"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 92).
?DOC("\n").
-spec footer(
    list(lustre@internals@vdom:attribute(OZX)),
    list(lustre@internals@vdom:element(OZX))
) -> lustre@internals@vdom:element(OZX).
footer(Attrs, Children) ->
    lustre@element:element(<<"footer"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 100).
?DOC("\n").
-spec header(
    list(lustre@internals@vdom:attribute(PAD)),
    list(lustre@internals@vdom:element(PAD))
) -> lustre@internals@vdom:element(PAD).
header(Attrs, Children) ->
    lustre@element:element(<<"header"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 108).
?DOC("\n").
-spec h1(
    list(lustre@internals@vdom:attribute(PAJ)),
    list(lustre@internals@vdom:element(PAJ))
) -> lustre@internals@vdom:element(PAJ).
h1(Attrs, Children) ->
    lustre@element:element(<<"h1"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 116).
?DOC("\n").
-spec h2(
    list(lustre@internals@vdom:attribute(PAP)),
    list(lustre@internals@vdom:element(PAP))
) -> lustre@internals@vdom:element(PAP).
h2(Attrs, Children) ->
    lustre@element:element(<<"h2"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 124).
?DOC("\n").
-spec h3(
    list(lustre@internals@vdom:attribute(PAV)),
    list(lustre@internals@vdom:element(PAV))
) -> lustre@internals@vdom:element(PAV).
h3(Attrs, Children) ->
    lustre@element:element(<<"h3"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 132).
?DOC("\n").
-spec h4(
    list(lustre@internals@vdom:attribute(PBB)),
    list(lustre@internals@vdom:element(PBB))
) -> lustre@internals@vdom:element(PBB).
h4(Attrs, Children) ->
    lustre@element:element(<<"h4"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 140).
?DOC("\n").
-spec h5(
    list(lustre@internals@vdom:attribute(PBH)),
    list(lustre@internals@vdom:element(PBH))
) -> lustre@internals@vdom:element(PBH).
h5(Attrs, Children) ->
    lustre@element:element(<<"h5"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 148).
?DOC("\n").
-spec h6(
    list(lustre@internals@vdom:attribute(PBN)),
    list(lustre@internals@vdom:element(PBN))
) -> lustre@internals@vdom:element(PBN).
h6(Attrs, Children) ->
    lustre@element:element(<<"h6"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 156).
?DOC("\n").
-spec hgroup(
    list(lustre@internals@vdom:attribute(PBT)),
    list(lustre@internals@vdom:element(PBT))
) -> lustre@internals@vdom:element(PBT).
hgroup(Attrs, Children) ->
    lustre@element:element(<<"hgroup"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 164).
?DOC("\n").
-spec main(
    list(lustre@internals@vdom:attribute(PBZ)),
    list(lustre@internals@vdom:element(PBZ))
) -> lustre@internals@vdom:element(PBZ).
main(Attrs, Children) ->
    lustre@element:element(<<"main"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 172).
?DOC("\n").
-spec nav(
    list(lustre@internals@vdom:attribute(PCF)),
    list(lustre@internals@vdom:element(PCF))
) -> lustre@internals@vdom:element(PCF).
nav(Attrs, Children) ->
    lustre@element:element(<<"nav"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 180).
?DOC("\n").
-spec section(
    list(lustre@internals@vdom:attribute(PCL)),
    list(lustre@internals@vdom:element(PCL))
) -> lustre@internals@vdom:element(PCL).
section(Attrs, Children) ->
    lustre@element:element(<<"section"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 188).
?DOC("\n").
-spec search(
    list(lustre@internals@vdom:attribute(PCR)),
    list(lustre@internals@vdom:element(PCR))
) -> lustre@internals@vdom:element(PCR).
search(Attrs, Children) ->
    lustre@element:element(<<"search"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 198).
?DOC("\n").
-spec blockquote(
    list(lustre@internals@vdom:attribute(PCX)),
    list(lustre@internals@vdom:element(PCX))
) -> lustre@internals@vdom:element(PCX).
blockquote(Attrs, Children) ->
    lustre@element:element(<<"blockquote"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 206).
?DOC("\n").
-spec dd(
    list(lustre@internals@vdom:attribute(PDD)),
    list(lustre@internals@vdom:element(PDD))
) -> lustre@internals@vdom:element(PDD).
dd(Attrs, Children) ->
    lustre@element:element(<<"dd"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 214).
?DOC("\n").
-spec 'div'(
    list(lustre@internals@vdom:attribute(PDJ)),
    list(lustre@internals@vdom:element(PDJ))
) -> lustre@internals@vdom:element(PDJ).
'div'(Attrs, Children) ->
    lustre@element:element(<<"div"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 222).
?DOC("\n").
-spec dl(
    list(lustre@internals@vdom:attribute(PDP)),
    list(lustre@internals@vdom:element(PDP))
) -> lustre@internals@vdom:element(PDP).
dl(Attrs, Children) ->
    lustre@element:element(<<"dl"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 230).
?DOC("\n").
-spec dt(
    list(lustre@internals@vdom:attribute(PDV)),
    list(lustre@internals@vdom:element(PDV))
) -> lustre@internals@vdom:element(PDV).
dt(Attrs, Children) ->
    lustre@element:element(<<"dt"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 238).
?DOC("\n").
-spec figcaption(
    list(lustre@internals@vdom:attribute(PEB)),
    list(lustre@internals@vdom:element(PEB))
) -> lustre@internals@vdom:element(PEB).
figcaption(Attrs, Children) ->
    lustre@element:element(<<"figcaption"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 246).
?DOC("\n").
-spec figure(
    list(lustre@internals@vdom:attribute(PEH)),
    list(lustre@internals@vdom:element(PEH))
) -> lustre@internals@vdom:element(PEH).
figure(Attrs, Children) ->
    lustre@element:element(<<"figure"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 254).
?DOC("\n").
-spec hr(list(lustre@internals@vdom:attribute(PEN))) -> lustre@internals@vdom:element(PEN).
hr(Attrs) ->
    lustre@element:element(<<"hr"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 259).
?DOC("\n").
-spec li(
    list(lustre@internals@vdom:attribute(PER)),
    list(lustre@internals@vdom:element(PER))
) -> lustre@internals@vdom:element(PER).
li(Attrs, Children) ->
    lustre@element:element(<<"li"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 267).
?DOC("\n").
-spec menu(
    list(lustre@internals@vdom:attribute(PEX)),
    list(lustre@internals@vdom:element(PEX))
) -> lustre@internals@vdom:element(PEX).
menu(Attrs, Children) ->
    lustre@element:element(<<"menu"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 275).
?DOC("\n").
-spec ol(
    list(lustre@internals@vdom:attribute(PFD)),
    list(lustre@internals@vdom:element(PFD))
) -> lustre@internals@vdom:element(PFD).
ol(Attrs, Children) ->
    lustre@element:element(<<"ol"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 283).
?DOC("\n").
-spec p(
    list(lustre@internals@vdom:attribute(PFJ)),
    list(lustre@internals@vdom:element(PFJ))
) -> lustre@internals@vdom:element(PFJ).
p(Attrs, Children) ->
    lustre@element:element(<<"p"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 291).
?DOC("\n").
-spec pre(
    list(lustre@internals@vdom:attribute(PFP)),
    list(lustre@internals@vdom:element(PFP))
) -> lustre@internals@vdom:element(PFP).
pre(Attrs, Children) ->
    lustre@element:element(<<"pre"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 299).
?DOC("\n").
-spec ul(
    list(lustre@internals@vdom:attribute(PFV)),
    list(lustre@internals@vdom:element(PFV))
) -> lustre@internals@vdom:element(PFV).
ul(Attrs, Children) ->
    lustre@element:element(<<"ul"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 309).
?DOC("\n").
-spec a(
    list(lustre@internals@vdom:attribute(PGB)),
    list(lustre@internals@vdom:element(PGB))
) -> lustre@internals@vdom:element(PGB).
a(Attrs, Children) ->
    lustre@element:element(<<"a"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 317).
?DOC("\n").
-spec abbr(
    list(lustre@internals@vdom:attribute(PGH)),
    list(lustre@internals@vdom:element(PGH))
) -> lustre@internals@vdom:element(PGH).
abbr(Attrs, Children) ->
    lustre@element:element(<<"abbr"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 325).
?DOC("\n").
-spec b(
    list(lustre@internals@vdom:attribute(PGN)),
    list(lustre@internals@vdom:element(PGN))
) -> lustre@internals@vdom:element(PGN).
b(Attrs, Children) ->
    lustre@element:element(<<"b"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 333).
?DOC("\n").
-spec bdi(
    list(lustre@internals@vdom:attribute(PGT)),
    list(lustre@internals@vdom:element(PGT))
) -> lustre@internals@vdom:element(PGT).
bdi(Attrs, Children) ->
    lustre@element:element(<<"bdi"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 341).
?DOC("\n").
-spec bdo(
    list(lustre@internals@vdom:attribute(PGZ)),
    list(lustre@internals@vdom:element(PGZ))
) -> lustre@internals@vdom:element(PGZ).
bdo(Attrs, Children) ->
    lustre@element:element(<<"bdo"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 349).
?DOC("\n").
-spec br(list(lustre@internals@vdom:attribute(PHF))) -> lustre@internals@vdom:element(PHF).
br(Attrs) ->
    lustre@element:element(<<"br"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 354).
?DOC("\n").
-spec cite(
    list(lustre@internals@vdom:attribute(PHJ)),
    list(lustre@internals@vdom:element(PHJ))
) -> lustre@internals@vdom:element(PHJ).
cite(Attrs, Children) ->
    lustre@element:element(<<"cite"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 362).
?DOC("\n").
-spec code(
    list(lustre@internals@vdom:attribute(PHP)),
    list(lustre@internals@vdom:element(PHP))
) -> lustre@internals@vdom:element(PHP).
code(Attrs, Children) ->
    lustre@element:element(<<"code"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 370).
?DOC("\n").
-spec data(
    list(lustre@internals@vdom:attribute(PHV)),
    list(lustre@internals@vdom:element(PHV))
) -> lustre@internals@vdom:element(PHV).
data(Attrs, Children) ->
    lustre@element:element(<<"data"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 378).
?DOC("\n").
-spec dfn(
    list(lustre@internals@vdom:attribute(PIB)),
    list(lustre@internals@vdom:element(PIB))
) -> lustre@internals@vdom:element(PIB).
dfn(Attrs, Children) ->
    lustre@element:element(<<"dfn"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 386).
?DOC("\n").
-spec em(
    list(lustre@internals@vdom:attribute(PIH)),
    list(lustre@internals@vdom:element(PIH))
) -> lustre@internals@vdom:element(PIH).
em(Attrs, Children) ->
    lustre@element:element(<<"em"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 394).
?DOC("\n").
-spec i(
    list(lustre@internals@vdom:attribute(PIN)),
    list(lustre@internals@vdom:element(PIN))
) -> lustre@internals@vdom:element(PIN).
i(Attrs, Children) ->
    lustre@element:element(<<"i"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 402).
?DOC("\n").
-spec kbd(
    list(lustre@internals@vdom:attribute(PIT)),
    list(lustre@internals@vdom:element(PIT))
) -> lustre@internals@vdom:element(PIT).
kbd(Attrs, Children) ->
    lustre@element:element(<<"kbd"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 410).
?DOC("\n").
-spec mark(
    list(lustre@internals@vdom:attribute(PIZ)),
    list(lustre@internals@vdom:element(PIZ))
) -> lustre@internals@vdom:element(PIZ).
mark(Attrs, Children) ->
    lustre@element:element(<<"mark"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 418).
?DOC("\n").
-spec q(
    list(lustre@internals@vdom:attribute(PJF)),
    list(lustre@internals@vdom:element(PJF))
) -> lustre@internals@vdom:element(PJF).
q(Attrs, Children) ->
    lustre@element:element(<<"q"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 426).
?DOC("\n").
-spec rp(
    list(lustre@internals@vdom:attribute(PJL)),
    list(lustre@internals@vdom:element(PJL))
) -> lustre@internals@vdom:element(PJL).
rp(Attrs, Children) ->
    lustre@element:element(<<"rp"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 434).
?DOC("\n").
-spec rt(
    list(lustre@internals@vdom:attribute(PJR)),
    list(lustre@internals@vdom:element(PJR))
) -> lustre@internals@vdom:element(PJR).
rt(Attrs, Children) ->
    lustre@element:element(<<"rt"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 442).
?DOC("\n").
-spec ruby(
    list(lustre@internals@vdom:attribute(PJX)),
    list(lustre@internals@vdom:element(PJX))
) -> lustre@internals@vdom:element(PJX).
ruby(Attrs, Children) ->
    lustre@element:element(<<"ruby"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 450).
?DOC("\n").
-spec s(
    list(lustre@internals@vdom:attribute(PKD)),
    list(lustre@internals@vdom:element(PKD))
) -> lustre@internals@vdom:element(PKD).
s(Attrs, Children) ->
    lustre@element:element(<<"s"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 458).
?DOC("\n").
-spec samp(
    list(lustre@internals@vdom:attribute(PKJ)),
    list(lustre@internals@vdom:element(PKJ))
) -> lustre@internals@vdom:element(PKJ).
samp(Attrs, Children) ->
    lustre@element:element(<<"samp"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 466).
?DOC("\n").
-spec small(
    list(lustre@internals@vdom:attribute(PKP)),
    list(lustre@internals@vdom:element(PKP))
) -> lustre@internals@vdom:element(PKP).
small(Attrs, Children) ->
    lustre@element:element(<<"small"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 474).
?DOC("\n").
-spec span(
    list(lustre@internals@vdom:attribute(PKV)),
    list(lustre@internals@vdom:element(PKV))
) -> lustre@internals@vdom:element(PKV).
span(Attrs, Children) ->
    lustre@element:element(<<"span"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 482).
?DOC("\n").
-spec strong(
    list(lustre@internals@vdom:attribute(PLB)),
    list(lustre@internals@vdom:element(PLB))
) -> lustre@internals@vdom:element(PLB).
strong(Attrs, Children) ->
    lustre@element:element(<<"strong"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 490).
?DOC("\n").
-spec sub(
    list(lustre@internals@vdom:attribute(PLH)),
    list(lustre@internals@vdom:element(PLH))
) -> lustre@internals@vdom:element(PLH).
sub(Attrs, Children) ->
    lustre@element:element(<<"sub"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 498).
?DOC("\n").
-spec sup(
    list(lustre@internals@vdom:attribute(PLN)),
    list(lustre@internals@vdom:element(PLN))
) -> lustre@internals@vdom:element(PLN).
sup(Attrs, Children) ->
    lustre@element:element(<<"sup"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 506).
?DOC("\n").
-spec time(
    list(lustre@internals@vdom:attribute(PLT)),
    list(lustre@internals@vdom:element(PLT))
) -> lustre@internals@vdom:element(PLT).
time(Attrs, Children) ->
    lustre@element:element(<<"time"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 514).
?DOC("\n").
-spec u(
    list(lustre@internals@vdom:attribute(PLZ)),
    list(lustre@internals@vdom:element(PLZ))
) -> lustre@internals@vdom:element(PLZ).
u(Attrs, Children) ->
    lustre@element:element(<<"u"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 522).
?DOC("\n").
-spec var(
    list(lustre@internals@vdom:attribute(PMF)),
    list(lustre@internals@vdom:element(PMF))
) -> lustre@internals@vdom:element(PMF).
var(Attrs, Children) ->
    lustre@element:element(<<"var"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 530).
?DOC("\n").
-spec wbr(list(lustre@internals@vdom:attribute(PML))) -> lustre@internals@vdom:element(PML).
wbr(Attrs) ->
    lustre@element:element(<<"wbr"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 537).
?DOC("\n").
-spec area(list(lustre@internals@vdom:attribute(PMP))) -> lustre@internals@vdom:element(PMP).
area(Attrs) ->
    lustre@element:element(<<"area"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 542).
?DOC("\n").
-spec audio(
    list(lustre@internals@vdom:attribute(PMT)),
    list(lustre@internals@vdom:element(PMT))
) -> lustre@internals@vdom:element(PMT).
audio(Attrs, Children) ->
    lustre@element:element(<<"audio"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 550).
?DOC("\n").
-spec img(list(lustre@internals@vdom:attribute(PMZ))) -> lustre@internals@vdom:element(PMZ).
img(Attrs) ->
    lustre@element:element(<<"img"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 556).
?DOC(" Used with <area> elements to define an image map (a clickable link area).\n").
-spec map(
    list(lustre@internals@vdom:attribute(PND)),
    list(lustre@internals@vdom:element(PND))
) -> lustre@internals@vdom:element(PND).
map(Attrs, Children) ->
    lustre@element:element(<<"map"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 564).
?DOC("\n").
-spec track(list(lustre@internals@vdom:attribute(PNJ))) -> lustre@internals@vdom:element(PNJ).
track(Attrs) ->
    lustre@element:element(<<"track"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 569).
?DOC("\n").
-spec video(
    list(lustre@internals@vdom:attribute(PNN)),
    list(lustre@internals@vdom:element(PNN))
) -> lustre@internals@vdom:element(PNN).
video(Attrs, Children) ->
    lustre@element:element(<<"video"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 579).
?DOC("\n").
-spec embed(list(lustre@internals@vdom:attribute(PNT))) -> lustre@internals@vdom:element(PNT).
embed(Attrs) ->
    lustre@element:element(<<"embed"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 584).
?DOC("\n").
-spec iframe(list(lustre@internals@vdom:attribute(PNX))) -> lustre@internals@vdom:element(PNX).
iframe(Attrs) ->
    lustre@element:element(<<"iframe"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 589).
?DOC("\n").
-spec object(list(lustre@internals@vdom:attribute(POB))) -> lustre@internals@vdom:element(POB).
object(Attrs) ->
    lustre@element:element(<<"object"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 594).
?DOC("\n").
-spec picture(
    list(lustre@internals@vdom:attribute(POF)),
    list(lustre@internals@vdom:element(POF))
) -> lustre@internals@vdom:element(POF).
picture(Attrs, Children) ->
    lustre@element:element(<<"picture"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 602).
?DOC("\n").
-spec portal(list(lustre@internals@vdom:attribute(POL))) -> lustre@internals@vdom:element(POL).
portal(Attrs) ->
    lustre@element:element(<<"portal"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 607).
?DOC("\n").
-spec source(list(lustre@internals@vdom:attribute(POP))) -> lustre@internals@vdom:element(POP).
source(Attrs) ->
    lustre@element:element(<<"source"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 614).
?DOC("\n").
-spec svg(
    list(lustre@internals@vdom:attribute(POT)),
    list(lustre@internals@vdom:element(POT))
) -> lustre@internals@vdom:element(POT).
svg(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"svg"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/html.gleam", 622).
?DOC("\n").
-spec math(
    list(lustre@internals@vdom:attribute(POZ)),
    list(lustre@internals@vdom:element(POZ))
) -> lustre@internals@vdom:element(POZ).
math(Attrs, Children) ->
    lustre@element:element(<<"math"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 632).
?DOC("\n").
-spec canvas(list(lustre@internals@vdom:attribute(PPF))) -> lustre@internals@vdom:element(PPF).
canvas(Attrs) ->
    lustre@element:element(<<"canvas"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 637).
?DOC("\n").
-spec noscript(
    list(lustre@internals@vdom:attribute(PPJ)),
    list(lustre@internals@vdom:element(PPJ))
) -> lustre@internals@vdom:element(PPJ).
noscript(Attrs, Children) ->
    lustre@element:element(<<"noscript"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 645).
?DOC("\n").
-spec script(list(lustre@internals@vdom:attribute(PPP)), binary()) -> lustre@internals@vdom:element(PPP).
script(Attrs, Js) ->
    lustre@element:element(<<"script"/utf8>>, Attrs, [text(Js)]).

-file("src/lustre/element/html.gleam", 652).
?DOC("\n").
-spec del(
    list(lustre@internals@vdom:attribute(PPT)),
    list(lustre@internals@vdom:element(PPT))
) -> lustre@internals@vdom:element(PPT).
del(Attrs, Children) ->
    lustre@element:element(<<"del"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 660).
?DOC("\n").
-spec ins(
    list(lustre@internals@vdom:attribute(PPZ)),
    list(lustre@internals@vdom:element(PPZ))
) -> lustre@internals@vdom:element(PPZ).
ins(Attrs, Children) ->
    lustre@element:element(<<"ins"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 670).
?DOC("\n").
-spec caption(
    list(lustre@internals@vdom:attribute(PQF)),
    list(lustre@internals@vdom:element(PQF))
) -> lustre@internals@vdom:element(PQF).
caption(Attrs, Children) ->
    lustre@element:element(<<"caption"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 678).
?DOC("\n").
-spec col(list(lustre@internals@vdom:attribute(PQL))) -> lustre@internals@vdom:element(PQL).
col(Attrs) ->
    lustre@element:element(<<"col"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 683).
?DOC("\n").
-spec colgroup(
    list(lustre@internals@vdom:attribute(PQP)),
    list(lustre@internals@vdom:element(PQP))
) -> lustre@internals@vdom:element(PQP).
colgroup(Attrs, Children) ->
    lustre@element:element(<<"colgroup"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 691).
?DOC("\n").
-spec table(
    list(lustre@internals@vdom:attribute(PQV)),
    list(lustre@internals@vdom:element(PQV))
) -> lustre@internals@vdom:element(PQV).
table(Attrs, Children) ->
    lustre@element:element(<<"table"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 699).
?DOC("\n").
-spec tbody(
    list(lustre@internals@vdom:attribute(PRB)),
    list(lustre@internals@vdom:element(PRB))
) -> lustre@internals@vdom:element(PRB).
tbody(Attrs, Children) ->
    lustre@element:element(<<"tbody"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 707).
?DOC("\n").
-spec td(
    list(lustre@internals@vdom:attribute(PRH)),
    list(lustre@internals@vdom:element(PRH))
) -> lustre@internals@vdom:element(PRH).
td(Attrs, Children) ->
    lustre@element:element(<<"td"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 715).
?DOC("\n").
-spec tfoot(
    list(lustre@internals@vdom:attribute(PRN)),
    list(lustre@internals@vdom:element(PRN))
) -> lustre@internals@vdom:element(PRN).
tfoot(Attrs, Children) ->
    lustre@element:element(<<"tfoot"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 723).
?DOC("\n").
-spec th(
    list(lustre@internals@vdom:attribute(PRT)),
    list(lustre@internals@vdom:element(PRT))
) -> lustre@internals@vdom:element(PRT).
th(Attrs, Children) ->
    lustre@element:element(<<"th"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 731).
?DOC("\n").
-spec thead(
    list(lustre@internals@vdom:attribute(PRZ)),
    list(lustre@internals@vdom:element(PRZ))
) -> lustre@internals@vdom:element(PRZ).
thead(Attrs, Children) ->
    lustre@element:element(<<"thead"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 739).
?DOC("\n").
-spec tr(
    list(lustre@internals@vdom:attribute(PSF)),
    list(lustre@internals@vdom:element(PSF))
) -> lustre@internals@vdom:element(PSF).
tr(Attrs, Children) ->
    lustre@element:element(<<"tr"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 749).
?DOC("\n").
-spec button(
    list(lustre@internals@vdom:attribute(PSL)),
    list(lustre@internals@vdom:element(PSL))
) -> lustre@internals@vdom:element(PSL).
button(Attrs, Children) ->
    lustre@element:element(<<"button"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 757).
?DOC("\n").
-spec datalist(
    list(lustre@internals@vdom:attribute(PSR)),
    list(lustre@internals@vdom:element(PSR))
) -> lustre@internals@vdom:element(PSR).
datalist(Attrs, Children) ->
    lustre@element:element(<<"datalist"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 765).
?DOC("\n").
-spec fieldset(
    list(lustre@internals@vdom:attribute(PSX)),
    list(lustre@internals@vdom:element(PSX))
) -> lustre@internals@vdom:element(PSX).
fieldset(Attrs, Children) ->
    lustre@element:element(<<"fieldset"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 773).
?DOC("\n").
-spec form(
    list(lustre@internals@vdom:attribute(PTD)),
    list(lustre@internals@vdom:element(PTD))
) -> lustre@internals@vdom:element(PTD).
form(Attrs, Children) ->
    lustre@element:element(<<"form"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 781).
?DOC("\n").
-spec input(list(lustre@internals@vdom:attribute(PTJ))) -> lustre@internals@vdom:element(PTJ).
input(Attrs) ->
    lustre@element:element(<<"input"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 786).
?DOC("\n").
-spec label(
    list(lustre@internals@vdom:attribute(PTN)),
    list(lustre@internals@vdom:element(PTN))
) -> lustre@internals@vdom:element(PTN).
label(Attrs, Children) ->
    lustre@element:element(<<"label"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 794).
?DOC("\n").
-spec legend(
    list(lustre@internals@vdom:attribute(PTT)),
    list(lustre@internals@vdom:element(PTT))
) -> lustre@internals@vdom:element(PTT).
legend(Attrs, Children) ->
    lustre@element:element(<<"legend"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 802).
?DOC("\n").
-spec meter(
    list(lustre@internals@vdom:attribute(PTZ)),
    list(lustre@internals@vdom:element(PTZ))
) -> lustre@internals@vdom:element(PTZ).
meter(Attrs, Children) ->
    lustre@element:element(<<"meter"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 810).
?DOC("\n").
-spec optgroup(
    list(lustre@internals@vdom:attribute(PUF)),
    list(lustre@internals@vdom:element(PUF))
) -> lustre@internals@vdom:element(PUF).
optgroup(Attrs, Children) ->
    lustre@element:element(<<"optgroup"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 818).
?DOC("\n").
-spec option(list(lustre@internals@vdom:attribute(PUL)), binary()) -> lustre@internals@vdom:element(PUL).
option(Attrs, Label) ->
    lustre@element:element(
        <<"option"/utf8>>,
        Attrs,
        [lustre@element:text(Label)]
    ).

-file("src/lustre/element/html.gleam", 823).
?DOC("\n").
-spec output(
    list(lustre@internals@vdom:attribute(PUP)),
    list(lustre@internals@vdom:element(PUP))
) -> lustre@internals@vdom:element(PUP).
output(Attrs, Children) ->
    lustre@element:element(<<"output"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 831).
?DOC("\n").
-spec progress(
    list(lustre@internals@vdom:attribute(PUV)),
    list(lustre@internals@vdom:element(PUV))
) -> lustre@internals@vdom:element(PUV).
progress(Attrs, Children) ->
    lustre@element:element(<<"progress"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 839).
?DOC("\n").
-spec select(
    list(lustre@internals@vdom:attribute(PVB)),
    list(lustre@internals@vdom:element(PVB))
) -> lustre@internals@vdom:element(PVB).
select(Attrs, Children) ->
    lustre@element:element(<<"select"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 847).
?DOC("\n").
-spec textarea(list(lustre@internals@vdom:attribute(PVH)), binary()) -> lustre@internals@vdom:element(PVH).
textarea(Attrs, Content) ->
    lustre@element:element(
        <<"textarea"/utf8>>,
        Attrs,
        [lustre@element:text(Content)]
    ).

-file("src/lustre/element/html.gleam", 854).
?DOC("\n").
-spec details(
    list(lustre@internals@vdom:attribute(PVL)),
    list(lustre@internals@vdom:element(PVL))
) -> lustre@internals@vdom:element(PVL).
details(Attrs, Children) ->
    lustre@element:element(<<"details"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 862).
?DOC("\n").
-spec dialog(
    list(lustre@internals@vdom:attribute(PVR)),
    list(lustre@internals@vdom:element(PVR))
) -> lustre@internals@vdom:element(PVR).
dialog(Attrs, Children) ->
    lustre@element:element(<<"dialog"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 870).
?DOC("\n").
-spec summary(
    list(lustre@internals@vdom:attribute(PVX)),
    list(lustre@internals@vdom:element(PVX))
) -> lustre@internals@vdom:element(PVX).
summary(Attrs, Children) ->
    lustre@element:element(<<"summary"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 880).
?DOC("\n").
-spec slot(list(lustre@internals@vdom:attribute(PWD))) -> lustre@internals@vdom:element(PWD).
slot(Attrs) ->
    lustre@element:element(<<"slot"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 885).
?DOC("\n").
-spec template(
    list(lustre@internals@vdom:attribute(PWH)),
    list(lustre@internals@vdom:element(PWH))
) -> lustre@internals@vdom:element(PWH).
template(Attrs, Children) ->
    lustre@element:element(<<"template"/utf8>>, Attrs, Children).
