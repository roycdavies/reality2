<!------------------------------------------------------------------------------------------------------
  A Sentant Card

  Author: Dr. Roy C. Davies
  Created: Jan 2024
  Contact: roycdavies.github.io
------------------------------------------------------------------------------------------------------->
<script context="module" lang="ts">
    import {Card, Content, Header} from "svelte-fomantic-ui";

    import type { Sentant } from './reality2_node.js';

    let sentant: Sentant = {id: "", name: ""};
    let 

    $: sentant : Sentant = sentant;
</script>
<!----------------------------------------------------------------------------------------------------->

<Card ui>
    <Content>
        <Header>{sentant.name}</Header>
    </Content>
</Card>