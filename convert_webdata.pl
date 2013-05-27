:- module(convert_webdata, [convert_to_xml/0]).

:- use_module(atoms, [atom/5]).

source_dir('data/www.webelements.com/').
target_dir('data/xml/').

convert_to_xml :-
    forall((atom(_S, _AN, Name, _, _), convert_to_xml(Name)), convert_to_xml(Name)).

convert_to_xml(Name) :-
    source_dir(Source),
    target_dir(Target),
    (   \+ exists_directory(Target) -> make_directory(Target) ; true ),
    atom_concat(Source, Name, AtomDir),
    atom_concat(AtomDir, '/*.html', Pattern),

    expand_file_name(Pattern, HtmlFiles),
    forall(member(HtmlFile, HtmlFiles),
           (
    file_base_name(HtmlFile, BaseName),
    file_name_extension(Without, 'html', BaseName),
    atom_concat(Target, Name, XmlDir),
    format(atom(XmlFile), '~w/~w.xml', [XmlDir, Without]),
    (    \+ exists_directory(XmlDir) -> make_directory(XmlDir) ; true ),

    load_xml_file(HtmlFile, XHtml),
    open(XmlFile, write, Stream, []),
    xml_write(Stream, XHtml, []),
    close(Stream)
           )
          ).

:- begin_tests(convert_webdata).

:- end_tests(convert_webdata).

