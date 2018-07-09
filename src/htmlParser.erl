-module(htmlParser).
-export([startCrawling/0,test/0,addHTMLTag/3,collectAttributeList/1]).

-define(HTML_IMAGE_TAG,img).
-define(HTML_TEXT_TAG,p).
-define(HTML_LINK_TAG,a).
-define(Other_HTML_TAG,other).
-define(NeedHTMLTags,[?HTML_IMAGE_TAG,?HTML_TEXT_TAG,?HTML_LINK_TAG]).
-define(OmmitHTMLTags,[br]).

test() ->collectAllHtmlContent("<div class=\"main\">
<!--....................start header....................-->
<div class=\"header\">
<div class=\"header_left\"> <a href=\"/\" ><img src=\"/wps/wcm/connect/f3ea75c8-0f26-4c87-b843-7411f3819913/logo.jpg?MOD=AJPERES&amp;CACHEID=f3ea75c8-0f26-4c87-b843-7411f3819913\" border=\"0\" alt=\"\" class=\"logo\" /></a></div>

<div class=\"header_right\"> ",[],[],false,createHTMLTable()).

collectAllHtmlContent([],HTMLTag,Val,_,EtsTableId)-> EtsTableId,ets:foldl(fun(X,_)-> io:format("~p\n",[X]),X end,[],EtsTableId),ok;

collectAllHtmlContent([$<|[$/|Sofar]],HTMLTag,Val,_,EtsTableId)  ->
		{NextHtmlContent,HMTLTAGVAL} = readColseTag(Sofar,[],Val,EtsTableId),
		collectAllHtmlContent(NextHtmlContent,[],HMTLTAGVAL,false,EtsTableId);

collectAllHtmlContent([$<|[$!|[$-|[$-|Sofar]]]],HTMLTag,Val,IsIneerTag,EtsTableId) ->
		collectAllHtmlContent(removeHTMLComments(Sofar),HTMLTag,Val,IsIneerTag,EtsTableId);

collectAllHtmlContent([$<|Sofar],HtmlTag,Val,false,EtsTableId)  ->
		insertHTMLTagVal(EtsTableId,HtmlTag,Val),
		collectAllHtmlContent(Sofar,[],[],true,EtsTableId); 

collectAllHtmlContent([$>|Sofar],HTMLTag,Val,true,EtsTableId)->
		Tag=processHtmlTag(HTMLTag,EtsTableId),
		collectAllHtmlContent(Sofar,Tag,Val,false,EtsTableId);

collectAllHtmlContent([$\"|Sofar],HTMLTag,Val,IsIneerTag,EtsTableId) ->
		collectAllHtmlContent(Sofar,HTMLTag,Val,IsIneerTag,EtsTableId);

collectAllHtmlContent([$\r|Sofar],HTMLTag,Val,IsIneerTag,EtsTableId) ->
		collectAllHtmlContent(Sofar,HTMLTag,Val,IsIneerTag,EtsTableId);

collectAllHtmlContent([$\n|Sofar],HTMLTag,Val,IsIneerTag,EtsTableId) ->
		collectAllHtmlContent(Sofar,HTMLTag,Val,IsIneerTag,EtsTableId);

collectAllHtmlContent([$\t|Sofar],HTMLTag,Val,IsIneerTag,EtsTableId) ->
		collectAllHtmlContent(Sofar,HTMLTag,Val,IsIneerTag,EtsTableId);

collectAllHtmlContent([Head|Sofar],HTMLTag,Val,IsIneerTag,EtsTableId) ->
		NewHtmlTag =addHTMLTag(IsIneerTag,HTMLTag,Head),
		NewVal = addHTMLVal(IsIneerTag,Val,Head),
		collectAllHtmlContent(Sofar,NewHtmlTag,NewVal,IsIneerTag,EtsTableId).

startCrawling() -> collectAllHtmlContent(getHtmlContent("http://www.airtel.in"),[],[],false,createHTMLTable()).

addHTMLTag(true,HtmlTagList,Element)-> HtmlTagList ++[Element];
addHTMLTag(_,HtmlTagList,_) -> HtmlTagList.

addHTMLVal(false,HtmlValList,Element) -> HtmlValList ++[Element];
addHTMLVal(_,HtmlValList,_) -> HtmlValList.


removeHTMLComments([$-|[$-|[$>|Sofar]]])->Sofar;
removeHTMLComments([_|Sofar]) -> removeHTMLComments(Sofar).

processHtmlTag(HtmlTag,EtsTableId)-> 	
					ElementList = re:split(HtmlTag," ",[{return,list}]),
					if length(ElementList) >=1 ->
						insertHTMLTagVal(EtsTableId,hd(ElementList),collectAttributeList(tl(ElementList)))
					end,hd(ElementList).

collectAttributeList(HtmlAttributeList) -> lists:foldl(fun(Element,Res) ->   
								ElementList = re:split(Element,"=",[{return,list}]),
								if length(ElementList) == 2 ->
									{list_to_atom(hd(ElementList)),
										hd(tl(ElementList))};					
								true-> []
								end
					end,[],HtmlAttributeList).

%% use only for colsing tag (</html>,</body>,</img>,</a>)this will update the ets table with HTML tag and value.

readColseTag([$>|Sofar],HtmlTag,Val,EtsTableId)-> 
		case insertHTMLTagVal(EtsTableId,HtmlTag,Val) of
		 true -> {Sofar,[]};
		 false ->{Sofar,HtmlTag}
		end;						
						
readColseTag([H|Sofar],HtmlTag,Val,EtsTableId)-> readColseTag(Sofar,HtmlTag++[H],Val,EtsTableId).

%% Manage Html data using ets table 

createHTMLTable() -> ets:new(test, [set]).

insertHTMLTagVal(TableId,HTML_TAG,Val)->   
					if HTML_TAG =/= [] andalso Val =/= [] ->
					  
						ets:insert(TableId,{list_to_atom(HTML_TAG),getHtmlTagVal(TableId,list_to_atom(HTML_TAG))++[Val]}),true;
					 true -> false
					end.
 
getHtmlTagVal(TableId,HTMLTAG)->
	case ets:lookup(TableId,HTMLTAG) of
		[] -> [];
		[{HTMLTAG,HList}] -> HList
	end.

%% get HTML content of web page

getHtmlContent(WebUrl)->
	inets:start(),
	HttpResponse = httpc:request(get, {WebUrl, [{"connection", "close"}]},[], []),
	case HttpResponse of
		{ok,{{"HTTP/1.1",200,"OK"},Header,Body}} -> 
			Body;
		Any  -> io:format(" Un-Handle Response ~p",[Any]),[]
	end.


