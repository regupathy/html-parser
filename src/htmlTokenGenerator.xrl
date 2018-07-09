Definitions.

TAG = [a-zA-Z0-9_\-]

Attr = [^>]

Rules.

[\s\t\r\n]+ : skip_token.

<!DOCTYPE[^>]*> : skip_token.

<!doctype[^>]*> : skip_token.

[<>\-]? : {token,{content,TokenChars}}.

<!-- : {token,{comment_start}}.

--> : {token,{comment_end}}.

\^ : {token,{content,"^"}}.

\$ : {token,{content,"$"}}.

<{TAG}+\s*> : {token,handle_start_tag(TokenChars)}.

<{TAG}+(\s*{TAG}+\s*=\s*{Attr}+\s*)*(/>|>) : {token,process_attributes(TokenChars)}. 

</{TAG}+\s*> : {token,handle_end_tag(TokenChars)}.

[^<^\-^>^\$^\^]* : {token,{content,remove_html_tags(TokenChars)}}.

Erlang code.

handle_start_tag(Tag)-> case lists:member(HTag=html_tag(Tag),all_html_tags()) of
				true ->  {start_tag,HTag,[]};
				false -> {content,Tag}
				end.

handle_end_tag(Tag)-> case lists:member(HTag=html_tag(Tag),all_html_tags()) of
				true ->  {end_tag,HTag};
				false -> {content,Tag}
				end.

html_tag(String) -> remove_tags_symbols(String,[]).
remove_tags_symbols([],Result) -> list_to_atom(lists:reverse(Result));
remove_tags_symbols([$/|Sofar],Result) -> remove_tags_symbols(Sofar,Result);
remove_tags_symbols([$<|Sofar],Result) -> remove_tags_symbols(Sofar,Result);
remove_tags_symbols([$>|Sofar],Result) -> remove_tags_symbols(Sofar,Result);
remove_tags_symbols([H|Sofar],Result) -> remove_tags_symbols(Sofar,[H|Result]).

remove_html_tags(List)->remove_html_tags(List,[]).
remove_html_tags([],Result) -> lists:reverse(Result);
remove_html_tags([$\t|Sofar],Res) ->remove_html_tags(Sofar,Res);
remove_html_tags([$\n|Sofar],Res) ->remove_html_tags(Sofar,Res);
remove_html_tags([H|T],Res) -> remove_html_tags(T,[H|Res]).

%% process on the html attribute list

process_attributes(Attr) ->
				{Tag_level,Attribute} = tag_level(lists:reverse(tl(Attr))),
				{TagName,AttriList} = attribute_tag_name(Attribute),
				case lists:member(TagName,all_html_tags()) of
				true ->  {Tag_level,TagName,format_attribute_list(split_by_equal(AttriList),[])};
				false -> {content,Attr}
				end.	
				
tag_level([$>|[$/|Rest]]) -> {start_end_tag,lists:reverse(Rest)}; 
tag_level([$>|Rest]) -> {start_tag,lists:reverse(Rest)};
tag_level(Rest)->{start_tag,lists:reverse(Rest)}.

attribute_tag_name(List) ->attribute_tag_name(List,[]). 
attribute_tag_name([],TagName) ->{list_to_atom(lists:reverse(TagName)),[]};
attribute_tag_name([$\s|Sofar],TagName)->{list_to_atom(lists:reverse(TagName)),Sofar};
attribute_tag_name([H|Sofar],TagName) -> attribute_tag_name(Sofar,[H|TagName]).

%% split_by_equal(ArrtibuteInput,IsAfterEqual,IsAfterDoubleQuate,Buffer,Result)
 
split_by_equal(List)-> split_by_equal(List,false,false,[],[]).

split_by_equal([],_,_,_,List) -> lists:reverse(List);
split_by_equal([$\s|Rest],false,false,Acc,List) -> split_by_equal(Rest,false,false,Acc,List);
split_by_equal([$=|Rest],false,false,Acc,List) -> split_by_equal(Rest,true,false,[],[lists:reverse(Acc)|List]);
split_by_equal([$\s|Rest],true,false,[],List) -> split_by_equal(Rest,true,false,[],List);
split_by_equal([$\s|Rest],true,false,Acc,List) -> split_by_equal(Rest,false,false,[],[Acc|List]);
split_by_equal([$"|Rest],true,false,_,List) -> split_by_equal(Rest,true,true,[],List);
split_by_equal([$"|Rest],true,true,Acc,List) -> split_by_equal(Rest,false,false,[],[lists:reverse(Acc)|List]);
split_by_equal([$'|Rest],true,false,_,List) -> split_by_equal(Rest,true,true,[],List);
split_by_equal([$'|Rest],true,true,Acc,List) -> split_by_equal(Rest,false,false,[],[lists:reverse(Acc)|List]);
split_by_equal([$\n|Rest],IsAfterEqual,IsAfterDoubleQuate,Acc,List) -> split_by_equal(Rest,IsAfterEqual,IsAfterDoubleQuate,Acc,List);
split_by_equal([$\t|Rest],IsAfterEqual,IsAfterDoubleQuate,Acc,List) -> split_by_equal(Rest,IsAfterEqual,IsAfterDoubleQuate,Acc,List);
split_by_equal([H|Rest],false,false,Acc,List) -> split_by_equal(Rest,false,false,[H|Acc],List);
split_by_equal([H|Rest],true,false,Acc,List) -> split_by_equal(Rest,true,false,[H|Acc],List);
split_by_equal([H|Rest],true,true,Acc,List) -> split_by_equal(Rest,true,true,[H|Acc],List).

format_attribute_list([],Result)->lists:reverse(Result);
format_attribute_list([Tag|[]],Result)->format_attribute_list([],[{list_to_atom(Tag),[]}|Result]);
format_attribute_list([Tag|[Val|Rest]],Result)-> format_attribute_list(Rest,[{list_to_atom(Tag),Val}|Result]).

all_html_tags() -> [a,abbr,acronym,address,applet,area,article,aside,audio,b,base,basefont,bdi,bdo,big,blockquote,body,br,
button,canvas,caption,center,cite,code,col,colgroup,datalist,dd,del,details,dfn,dialog,dir,'div',dl,
dt,em,embed,fieldset,figcaption,figure,font,footer,form,frame,frameset,head,header,hgroup,h1, h2,h3,h4,h5,h6,hr,html,i,iframe,img,input,ins,kbd,keygen,label,legend,li,link,main,map,mark,
menu,menuitem,meta,meter,nav,noframes,noscript,object,ol,optgroup,option,output,p,param,pre,progress,q,rp,
rt,ruby,s,samp,script,section,select,small,source,span,strike,strong,style,sub,summary,sup,table,tbody,td,
textarea,tfoot,th,thead,time,title,tr,track,tt,u,ul,var,video,wbr].

