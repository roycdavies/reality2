% ----------------------------------------------------------------------------------------------------
% Copyright Imersia Ltd 2019
% Author: Dr. Roy C. Davies
% Unit tests for the Datatypes.
% ----------------------------------------------------------------------------------------------------
-module(test_types).

-include("../imersia_datatypes.hrl").

-export([test/0]).

test() ->

    erlang:display("*** Testing Automations ***"),

    Action1Text = <<"{\"command\":\"set\",\"parameters\":{\"key\":\"test\",\"value\":1}}">>,
    Action1JSON = imersia_misc:safely_decode_json(Action1Text),
    Action1 = imersia_misc:json_to_record(action, Action1JSON),

    Action2Text = <<"{\"command\":\"send\",\"parameters\":{\"geobotid\":\"fred\",\"event\":\"test\",\"delay\":23,\"parameters\":[]}}">>,
    Action2JSON = imersia_misc:safely_decode_json(Action2Text),
    Action2 = imersia_misc:json_to_record(action, Action2JSON),

    Transition1Text = <<"{\"state\":\"on\",\"event\":\"turnoff\",\"newstate\":\"off\",\"actions\":[", Action1Text/binary, ",", Action2Text/binary, "]}">>,
    Transition1JSON = imersia_misc:safely_decode_json(Transition1Text),
    Transition1 = imersia_misc:json_to_record(transition, Transition1JSON),

    Transition2Text = <<"{\"state\":\"off\",\"event\":\"turnon\",\"newstate\":\"on\",\"actions\":[", Action2Text/binary, ",", Action1Text/binary, "]}">>,
    Transition2JSON = imersia_misc:safely_decode_json(Transition2Text),
    Transition2 = imersia_misc:json_to_record(transition, Transition2JSON),

    Automation = #automation{
        name = <<"Test Automation">>,
        description = <<"A Description">>,
        transitions = [Transition1, Transition2]
    },

    AutomationText = <<"{\"name\":\"Test Automation\",\"description\":\"A Description\",\"transitions\":[", Transition1Text/binary, ",", Transition2Text/binary, "]}">>,
    AutomationJSON = imersia_misc:safely_decode_json(AutomationText),
    Automation = imersia_misc:json_to_record(automation, AutomationJSON),

    erlang:display(Automation),

    Action1Converted = imersia_misc:record_to_json(Action1, true),
    Action2Converted = imersia_misc:record_to_json(Action2, true),

    Transition1Converted = imersia_misc:record_to_json(Transition1, true),
    Transition2Converted = imersia_misc:record_to_json(Transition2, true),

    AutomationConverted = imersia_misc:record_to_json(Automation, true),

    AutomationText = imersia_misc:safely_encode_json(AutomationConverted),
    Transition1Text = imersia_misc:safely_encode_json(Transition1Converted),
    Transition2Text = imersia_misc:safely_encode_json(Transition2Converted),
    Action1Text = imersia_misc:safely_encode_json(Action1Converted),
    Action2Text = imersia_misc:safely_encode_json(Action2Converted),

    AutomationMap = imersia_misc:safely_convert_to_map(AutomationJSON),
    erlang:display(AutomationMap),

    erlang:display(imersia_misc:safely_convert_from_map(AutomationMap)),

    {ok, all_tests_passed}.
