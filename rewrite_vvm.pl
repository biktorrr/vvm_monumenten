:- module(vvm_rewrite,
	  [ rewrite_vvm/0,
	    rewrite_vvm/1,
	    rewrite_vvm/2,
	    list_rules/0
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(xmlrdf/rdf_convert_util)).
:- use_module(library(xmlrdf/cvt_vocabulary)).
:- use_module(library(xmlrdf/rdf_rewrite)).
:- use_module(library(semweb/rdf_litindex)).

:- debug(rdf_rewrite).

%%	rewrite
%
%	Apply all rules on the graph =data=

rewrite_vvm :-
	rdf_rewrite(vvm).

%%	rewrite(+Rule)
%
%	Apply the given rule on the graph =data=

rewrite_vvm(Rule) :-
	rdf_rewrite(vvm, Rule).

%%	rewrite(+Graph, +Rule)
%
%	Apply the given rule on the given graph.

rewrite_vvm(Graph, Rule) :-
	rdf_rewrite(Graph, Rule).

%%	list_rules
%
%	List the available rules to the console.

list_rules :-
	rdf_rewrite_rules.

:- discontiguous
	rdf_mapping_rule/5.



rule1
@@
{S, vvm:field, Field},
{Field, vvm:name, PredName},
{Field, rdf:value, Value}
<=>
literal_to_id([PredName], vvm, PURI),
{S, PURI, Value}.

remove_field
@@
{_S, rdf:type, vvm:'Field'}
<=>
true.
remove_field
@@
{_S, vvm:field, _}
<=>
true.

remove_name
@@
{_, vvm:name, _}
<=>
true.

imageURI
@@
{S, vvm:image, literal(Ref)}
<=>
concat_atom(['http://www.4en5mei.nl', Ref],URI),
{S, vvm:image, URI}.

assign_uris
@@
{S, vvm:datasource_id, literal(ID)}\
{S}
<=>
literal_to_id(['monument-',ID], vvm, URI),
	{URI}.
cleaning
@@
{_S, vvm:description, _}
<=>
true.

unveilingdate
@@
{S, vvm:unveilingday, literal(Day)},
{S, vvm:unveilingmonth, literal(Month)},
{S, vvm:unveilingyear, literal(Year)}
==>
Day \= '-1',
Month \= '-1',
Year \= '-1',
concat_atom([Day, '-', Month, '-', Year], Date),
{S, vvm:unveilingdate, literal(Date)}.

link_to_geonames
@@
{Geo, 'http://www.geonames.org/ontology#name', literal(City)}\
{S, vvm:city, literal(City)}
<=>
{S, vvm:city, Geo}.

link_to_geonames
@@
{Geo, 'http://www.geonames.org/ontology#name', literal(City)}\
{S, vvm:community, literal(City)}
<=>
{S, vvm:community, Geo}.


address_to_concept
@@
{S, vvm:address, literal(Ad)}
<=>
literal_to_id(['adress-',Ad],vvm,URI),
{URI, rdf:type, skos:'Concept'},
{URI, skos:inScheme, vvm:'VVMConceptScheme'},
{URI, skos:prefLabel, literal(Ad)},
{S, vvm:address, URI}.



artist
@@
{S, vvm:artist, literal(Artist)}
<=>
atomic_list_concat(SubList,',',Artist),
forall(member(MG,SubList),
       rdf_assert(S, vvm:artist1, literal(MG),vvm)).


artist
@@
{S, vvm:artist1, literal(Artist)}
<=>
{S, vvm:artist, bnode([rdf:value = literal(Artist)])}.

artist
@@
{_S, vvm:artist, BN}\
	{BN, rdf:value, literal(Artist)}
	<=>
getyear(Artist, ArtistName, Years),
name_to_id(ArtistName,vvm,ArtistURI),
	{BN, rdf:value, ArtistURI},
	{ArtistURI, foaf:name, literal(ArtistName)},
	{ArtistURI, foaf:age, literal(Years)},
	{ArtistURI, rdf:type, foaf:'Person'} .


artist
@@
{_S, vvm:artist, BN}\
	{BN, rdf:value, literal(Artist)}
	<=>
getqualifier(Artist, ArtistName, Qual),
name_to_id(ArtistName,vvm,ArtistURI),
	{BN, rdf:value, ArtistURI},
	{ArtistURI, foaf:name, literal(ArtistName)},
	{BN, vvm:qualifier, literal(Qual)},
	{ArtistURI, rdf:type, foaf:'Person'} .

artist
@@
{_S, vvm:artist, BN}\
	{BN, rdf:value, literal(ArtistName)}
	<=>
name_to_id(ArtistName,vvm,ArtistURI),
	{BN, rdf:value, ArtistURI},
	{ArtistURI, foaf:name, literal(ArtistName)},
	{ArtistURI, rdf:type, foaf:'Person'} .


getyear(Artist, ArtistName, Years):-
	atomic_list_concat([ArtistName,Year1],'(',Artist),
	atom_codes(Year1,[Num|_]),
	Num > 47,
	Num < 58,
	atomic_list_concat(['(',Year1],Years).


getqualifier(Artist, ArtistName, Qual):-
	atomic_list_concat([ArtistName,Year1],'(',Artist),
	not(atom_codes(Year1,[49|_])),
	atomic_list_concat(['(',Year1],Qual).


/* -- replaced by manual files
memorialgroups
@@
{S, vvm:memorialgroups, literal(Groups)}
<=>
atomic_list_concat(SubList,',',Groups),
forall(member(MG,SubList),
       rdf_assert(S, vvm:memorialgroups, literal(MG),[graph(vvm)])).

memorialgroups
@@
{S, vvm:memorialgroups, literal(Groups)}
<=>
atomic_list_concat(SubList,'.',Groups),
forall(member(MG,SubList),
       rdf_assert(S, vvm:memorialgroups, literal(MG),[graph(vvm)])).

memorialgroups_to_thes
@@
{Thes, skos:inScheme, vvm:'MemorialGroups'},
	{Thes, skos:note, literal(ID)}\
{S, vvm:memorialgroups, literal(ID)}
<=>
{S,vvm:memorialgroup, Thes}.
*/

