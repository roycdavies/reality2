<!------------------------------------------------------------------------------------------------------
  A Sentant Card

  Author: Dr. Roy C. Davies
  Created: Jan 2024
  Contact: roycdavies.github.io
------------------------------------------------------------------------------------------------------->
<script lang="ts">
    import {Card, Content, Header, Image} from "svelte-fomantic-ui";

    import type { Sentant } from './reality2_node.js';

    export let sentant: Sentant = {name: "Sentant Name", id: "Sentant ID"};
    
</script>
<!----------------------------------------------------------------------------------------------------->

<Card ui>
    <Image ui large src="/images/globe.png"/>
    <Content>
        <Header>Name : {sentant.name}</Header>
    </Content>
    <Content extra>
        <p>{sentant.id}</p>
    </Content>
</Card>