% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Description: Convert channels and geobots to GeoJSON
% ----------------------------------------------------------------------------------------------------

%% @hidden

-module(imersia_geojson).

-include("../imersia_datatypes.hrl").

-export([channel/3, geobot/3, channels/1, channels/2]).

channel(Channel, Metadata, Geobots) ->
    #{
        <<"type">> => <<"FeatureCollection">>,
        <<"features">> => Geobots,
        <<"details">> => #{
            <<"channelid">> => Channel#channel.channelid,
            <<"ownerid">> => Channel#channel.ownerid,
            <<"name">> => Channel#channel.name,
            <<"description">> => Channel#channel.description,
            <<"class">> => Channel#channel.class,
            <<"imageurl">> => Channel#channel.imageurl,
            <<"hidden">> => Channel#channel.hidden,
            <<"created">> => Channel#channel.created,
            <<"modified">> => Channel#channel.modified,
            <<"metadata">> => metadata(Metadata)
        }
    }.

channels(Channels) ->
    #{
        <<"type">> => <<"FeatureCollection">>,
        <<"features">> => [],
        <<"channels">> => Channels
    }.

channels(Channels, Geobots) ->
    #{
        <<"type">> => <<"FeatureCollection">>,
        <<"features">> => Geobots,
        <<"channels">> => Channels
    }.

geobot(Geobot, Metadata, Commands) ->
    #{
        <<"type">> => <<"Feature">>,
        <<"geometry">> => #{
            <<"type">> => <<"Point">>,
            <<"coordinates">> => [
                Geobot#geobot.location#location.longitude,
                Geobot#geobot.location#location.latitude,
                Geobot#geobot.location#location.altitude
            ]
        },
        <<"properties">> => #{
            <<"geobotid">> => Geobot#geobot.geobotid,
            <<"channelid">> => Geobot#geobot.channelid,
            <<"ownerid">> => Geobot#geobot.ownerid,
            <<"name">> => Geobot#geobot.name,
            <<"description">> => Geobot#geobot.description,
            <<"location">> => #{
                <<"latitude">> => Geobot#geobot.location#location.latitude,
                <<"longitude">> => Geobot#geobot.location#location.longitude,
                <<"altitude">> => Geobot#geobot.location#location.altitude,
                <<"geohash">> => Geobot#geobot.location#location.geohash
            },
            <<"class">> => Geobot#geobot.class,
            <<"imageurl">> => Geobot#geobot.imageurl,
            <<"hidden">> => Geobot#geobot.hidden,
            <<"created">> => Geobot#geobot.created,
            <<"modified">> => Geobot#geobot.modified,
            <<"metadata">> => metadata(Metadata),
            <<"commands">> => Commands
        }
    }.


metadata([]) -> [];
metadata([Metadata | Metadatas]) ->
    [#{
        <<"metadataid">> => Metadata#metadata.metadataid,
        <<"key">> => Metadata#metadata.key,
        <<"value">> => Metadata#metadata.value
    } | metadata(Metadatas)].
