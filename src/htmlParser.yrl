Nonterminals tag comments tags improperTag improperTags.
Terminals comment_start comment_end start_tag end_tag start_end_tag content.
Rootsymbol tags.

tag -> comments : {[],[]}.

tag -> content : {[],element(2,'$1')}.

tag -> start_end_tag : {value_of('$1'),[]}.

tag -> start_tag end_tag : {value_of('$1'),[]}.

tag -> start_tag tags end_tag : {add_content(value_of('$1',element(2,'$2')),element(1,'$2')),[]}.

tags -> tag : '$1'.

tags -> tag tags : {add_tags('$1','$2'),add_contents('$1','$2')}.

comments -> comment_start improperTags comment_end : {comment,'$2'}.

improperTag -> start_tag : '$1'.

improperTag -> end_tag : '$1'.

improperTag -> content : '$1'.

improperTags -> improperTag : '$1'.

improperTags -> improperTag improperTags : ['$1','$2'].

Erlang code.

value_of({start_tag,Tag,Attri}) ->  {Tag,Attri,[]};
value_of({start_end_tag,Tag,Attri}) ->  {Tag,Attri,[]}.
value_of({start_tag,Tag,Attri},Content) ->  {Tag,Attri,Content}.

add_content([],Content) -> Content;
add_content(Item,[]) -> [Item];
add_content(Item,Content) -> [Item|Content].

add_tags({Tag1,_},{[],_}) ->  Tag1;
add_tags({[],_},{Tag2,_}) ->  Tag2;
add_tags({Tag1,_},{Tag2,_})when is_list(Tag2) ->   merge_tags(Tag1,Tag2);
add_tags({Tag1,_},{Tag2,_}) ->  merge_tags(Tag1,[Tag2]).

merge_tags([],Result) -> Result;
merge_tags(Tags,Result)when is_list(Tags) -> merge_tags(tl(Tags),[hd(Tags)|Result]);
merge_tags(Tag,Result) -> [Tag|Result].

add_contents({_,Content1},{_,Content2}) -> lists:concat([Content1|[Content2]]).









