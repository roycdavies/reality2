<!------------------------------------------------------------------------------------------------------
  Types for the Finite State Machines used in the application.

  Author: Dr. Roy C. Davies
  Created: June 2023
  Contact: roy.c.davies@ieee.org
------------------------------------------------------------------------------------------------------->
<script context="module" lang="ts">

    import type { Item, Session } from './Graphql.svelte';

    export type QueryVars = {
        name : string,
        qrcode : string,
        qrsearch : string,
        search : string
    };

    export type AppVars = {
        session : Session,
        item : Item,
        message : string
    }

    export enum AppStates { 
        _='*', 
        LOAD='load', 
        INIT='init', 
        VIEW='view'
    };
    export enum AppEvents {
        LOAD_QUERY_STRINGS='loadQueryStrings'
    };

    export enum LoginStates {
        _='*',
        LOAD='load'
    }
    export enum LoginEvents {
        INITIALISE="initialise"
    }

</script>
<!----------------------------------------------------------------------------------------------------->