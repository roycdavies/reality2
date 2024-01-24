<!------------------------------------------------------------------------------------------------------
  Main controlling App for Booking System

  Author: Dr. Roy C. Davies
  Created: June 2023
  Contact: roy.c.davies@ieee.org
------------------------------------------------------------------------------------------------------->
<script lang="ts">
    import { behavior, Message, Image, Grid, Column, Header, Content, Button } from "svelte-fomantic-ui";

    import { ApolloClient, InMemoryCache } from '@apollo/client';
    import { setClient } from 'svelte-apollo';

    import type { QueryVars, AppVars } from './lib/Types.svelte';
    import { AppStates, AppEvents, LoginStates, LoginEvents } from './lib/Types.svelte';
    import Automation from './lib/FiniteStateMachine';

   
    import { Itemtype, Usertype } from "./lib/Graphql.svelte";
    import type { Item as ItemT, Session } from './lib/Graphql.svelte';
    import { getQueryStringVal } from './lib/Querystring.svelte';

    import { onMount } from 'svelte';

    // -------------------------------------------------------------------------------------------------
    // Window width
    // -------------------------------------------------------------------------------------------------
    let windowWidth: number = 0;
    let numCols: number = 3;

    const setNumCols = () => { 
        if (windowWidth < 512) return 1;
        else if (windowWidth < 768) return 2;
        else if (windowWidth < 1024) return 3;
        else return 4;
    };

    const setWindowWidth = () => { windowWidth = window.innerWidth; numCols = setNumCols(); };

    onMount(() => {
        windowWidth = window.innerWidth; numCols = setNumCols();	
        window.addEventListener('resize', setWindowWidth);

        return () => { 
            window.removeEventListener('resize', setWindowWidth);  
        }
    });
    // -------------------------------------------------------------------------------------------------



    // -------------------------------------------------------------------------------------------------
    // Prevent the user from leaving the page
    // -------------------------------------------------------------------------------------------------
    function beforeunload(event: BeforeUnloadEvent) {
        if ((AppC.currentState !== AppStates.QRCODE) && (AppC.currentState !== AppStates.QRSEARCH))
        {
            event.preventDefault();
            return (event.returnValue = "");
        }
    }
    // -------------------------------------------------------------------------------------------------



    // -------------------------------------------------------------------------------------------------
    // UI Controlling Variables
    // -------------------------------------------------------------------------------------------------

    // -------------------------------------------------------------------------------------------------



    // -------------------------------------------------------------------------------------------------
    // GraphQL
    // -------------------------------------------------------------------------------------------------
    // GraphQL client setup 
    const client = new ApolloClient({
        uri:  '/api',
        cache: new InMemoryCache()
    });
    setClient(client);
    // -------------------------------------------------------------------------------------------------



    // -------------------------------------------------------------------------------------------------
    // The main UX / App App Controller
    // -------------------------------------------------------------------------------------------------
    let createAppKey = (currentState : AppStates, triggerEvent : AppEvents) => currentState + '|' + triggerEvent;
    let AppC = new Automation<AppStates, AppEvents> (AppStates.LOAD, createAppKey, AppStates._);

    // -------------------------------------------------------------------------------------------------
    // Transitions
    // -------------------------------------------------------------------------------------------------
    // Check and load the query strings as these define which page to then show
    AppC.add_transition(AppStates.LOAD,                     AppEvents.LOAD_QUERY_STRINGS,       AppStates.INIT,                         loadQueryStrings);

    // -------------------------------------------------------------------------------------------------
    // Actions
    // -------------------------------------------------------------------------------------------------

    // Load the values from the query strings
    function loadQueryStrings (_ : {}) {
        // queryVars.name = getQueryStringVal("item");

        updateUI;     
    }

    // Do (almost) nothing
    function doNothing (_ : {} = {}) { updateUI(); }

    // Force the UI to update (and update a few variables)
    function updateUI (_ : {} = {}) { 
    };

    // ------------------------------------------------------------------------------------------------- 
</script>
<!----------------------------------------------------------------------------------------------------->



<!------------------------------------------------------------------------------------------------------
Styles
------------------------------------------------------------------------------------------------------->
<style>

</style>
<!----------------------------------------------------------------------------------------------------->



<!------------------------------------------------------------------------------------------------------
Layout
------------------------------------------------------------------------------------------------------->
<svelte:window on:beforeunload={beforeunload} />
<main>

    <Button ui red>Test</Button>

</main>
<!----------------------------------------------------------------------------------------------------->