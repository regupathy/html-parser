-module(htmlParserTester).
-export([parse/1,test/0,parseString/1,token/1]).

parse(FileName) -> parseString(read_file(FileName)).

token(FileName) -> htmlTokenGenerator:string(read_file(FileName)).

parseString(String) -> {ok,Tokens,_} = htmlTokenGenerator:string(String),
				
				io:format("Before Filter : ~p\n",[print_all_tags(Tokens,0,0)]),
				FilterToken =verify_open_close_html_tag(Tokens,[],[],true),
				io:format("After Filter : ~p\n",[print_all_tags(FilterToken,0,0)]),
				clear_file("log"),
				write_in_file("log",FilterToken),
				{ok,{ParseData,_}}=htmlParser:parse(FilterToken),
				clear_file("result"),
				write_in_file("result",ParseData).

print_all_tags([],StartTagCount,EndTagCount)->{StartTagCount,EndTagCount};
print_all_tags([{start_tag,_Tag,_}|Rest],StartTagCount,EndTagCount) ->print_all_tags(Rest,StartTagCount+1,EndTagCount);
print_all_tags([{end_tag,_}|Rest],StartTagCount,EndTagCount) ->print_all_tags(Rest,StartTagCount,EndTagCount+1);
print_all_tags([_|Rest],StartTagCount,EndTagCount) ->print_all_tags(Rest,StartTagCount,EndTagCount).

test() -> parseString(read_file("html_file.html")).

write_in_file(_,[]) -> ok;
%write_in_file(Path,[{content,_}|Rest]) -> write_in_file(Path,Rest);
write_in_file(Path,[H|Rest]) -> file:write_file(Path,io_lib:fwrite("~p.\n",[H]),[append]),write_in_file(Path,Rest).

clear_file(File) -> file:write_file(File,io_lib:fwrite("",[]),[write]).

read_file(FileName)-> Device = open_file(FileName, read),List= read(Device),convertToList(List,[]).

open_file(FileName, Mode) ->
    {ok, Device} = file:open(FileName, [Mode, binary]),
    Device.

convertToList([],Res) -> lists:concat(lists:reverse(Res));
convertToList([H|Sofar],Res) -> convertToList(Sofar,[binary_to_list(H)|Res]).

read(File) ->
    case file:read_line(File) of
        {ok, Data} -> [Data | read(File)];
        eof       -> []
    end.

verify_open_close_html_tag([],[],Result,_)->lists:reverse(Result);
verify_open_close_html_tag([],Tags,Result,_) -> verify_open_close_html_tag([],[],add_item(Tags,Result),true);
verify_open_close_html_tag([H={start_tag,Tag,_}|Sofar],Tags,Result,true) -> verify_open_close_html_tag(Sofar,[Tag|Tags],[H|Result],true);
verify_open_close_html_tag([{end_tag,_Tag}|Sofar],[],Result,true) ->  verify_open_close_html_tag(Sofar,[],Result,true);
verify_open_close_html_tag([H={end_tag,Tag}|Sofar],[Tag|RestTag],Result,true) -> verify_open_close_html_tag(Sofar,RestTag,[H|Result],true);

verify_open_close_html_tag([H={end_tag,_Tag}|Sofar],Tags,Result,true)  ->  verify_open_close_html_tag([H|Sofar],tl(Tags),[{end_tag,hd(Tags)}|Result],true);

verify_open_close_html_tag([H={comment_start}|Sofar],Tags,Result,true) -> verify_open_close_html_tag(Sofar,Tags,[H|Result],false);
verify_open_close_html_tag([H={comment_end}|Sofar],Tags,Result,false) -> verify_open_close_html_tag(Sofar,Tags,[H|Result],true);

verify_open_close_html_tag([H={comment_start}|Sofar],Tags,Result,false) -> verify_open_close_html_tag(Sofar,Tags,[H|Result],false);
verify_open_close_html_tag([H={comment_end}|Sofar],Tags,Result,Any) -> verify_open_close_html_tag(Sofar,Tags,[H|Result],Any);

verify_open_close_html_tag([H|Sofar],Tags,Result,NotInsideCommends) -> verify_open_close_html_tag(Sofar,Tags,[H|Result],NotInsideCommends).

add_item([],List)->List;
add_item([Item|Rest],List)-> add_item(Rest,[{end_tag,Item}|List]).



