:- module(viervijf_run,
	  [ run_vvm/0,
	    clean_vvm/0
	  ]).

user:file_search_path(data,       '../data/niod').

:- use_module(library(semweb/rdf_db)).

:- rdf_register_ns(vvm,	   'http://purl.org/collections/nl/viervijfmei/').
:- rdf_register_ns(oai,	'http://www.openarchives.org/OAI/2.0/').
:- rdf_register_ns(skos, 'http://www.w3.org/2004/02/skos/core#').

:- use_module([ library(xmlrdf/xmlrdf),
		library(semweb/rdf_cache),
		library(semweb/rdf_library),
		library(semweb/rdf_turtle_write)
	      ]).
:- use_module(rewrite_vvm).

load_ontologies :-
	rdf_load_library(dc),
	rdf_load_library(skos),
	rdf_load_library(rdfs),
	rdf_load_library(owl),
	rdf_load(data('../geonames/NL.rdf'),[graph(geonames_nl)]).

:- initialization			% run *after* loading this file
	rdf_set_cache_options([ global_directory('cache/rdf'),
				create_global_directory(true)
			      ]),
	load_ontologies.

load_vvm:-
        absolute_file_name(data('../45mei/t_oorlogsmonumenten_view.xml'), File,
			   [ access(read)
			   ]),
	load(File).

load(File) :-
	rdf_current_ns(vvm, Prefix),
	load_xml_as_rdf(File,
			[ dialect(xmlns),
			  unit(row),
			  prefix(Prefix),
			  graph(vvm)
			]).


run_vvm:-
	load_vvm,
	rewrite_vvm,
	save_vvm.

save_vvm:-
	absolute_file_name(data('../45mei/vvm_data.ttl'), File,
			   [ access(write)
			   ]),
	rdf_save_turtle(File,[graph(vvm)]).


clean_vvm:-
	rdf_retractall(_,_,_,vvm).













