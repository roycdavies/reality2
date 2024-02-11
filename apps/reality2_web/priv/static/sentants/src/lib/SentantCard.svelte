<!------------------------------------------------------------------------------------------------------
  A Sentant Card

  Author: Dr. Roy C. Davies
  Created: Jan 2024
  Contact: roycdavies.github.io
------------------------------------------------------------------------------------------------------->
<script context="module" lang="ts">
    import {Card, Content, Header} from "svelte-fomantic-ui";

    import type { Sentant } from './reality2_node.js';

    export let sentant: Sentant;

</script>
<!----------------------------------------------------------------------------------------------------->

<Card ui>
    <Content>
        <Header>Name : {sentant.name}</Header>
    </Content>
</Card>