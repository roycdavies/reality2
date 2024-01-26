<!------------------------------------------------------------------------------------------------------
  Main controlling App for Booking System

  Author: Dr. Roy C. Davies
  Created: June 2023
  Contact: roy.c.davies@ieee.org
------------------------------------------------------------------------------------------------------->
<script lang="ts">
    import { behavior, Message, Image, Grid, Column, Header, Content, Button } from "svelte-fomantic-ui";

    import reality2_node from "./lib/reality2_node";

    import type { QueryVars, AppVars } from './lib/Types.svelte';
    import { AppStates, AppEvents } from './lib/Types.svelte';
    import Automation from './lib/FiniteStateMachine';

   
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
        event.preventDefault();
        return (event.returnValue = "");
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
    let reality2Node = new reality2_node("https://localhost:4001");

    $: allSentants = reality2Node.sentantAll();

    function loadAll() {
        reality2Node.sentantAll();
    }
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

    <Button ui red on:click={loadAll}>Load</Button>

    {#await allSentants}
        <p>Loading...</p>
    {:then response}
    {#each response.data.sentantAll as sentant}
        <p>{sentant.id}</p>
        <p>{sentant.name}</p>
    {/each}
    {:catch error}
        <p>Error: {error.message}</p>
    {/await}
</main>
<!----------------------------------------------------------------------------------------------------->