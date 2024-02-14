<!------------------------------------------------------------------------------------------------------
  A Sentant Card

  Author: Dr. Roy C. Davies
  Created: Feb 2024
  Contact: roycdavies.github.io
------------------------------------------------------------------------------------------------------->
<script lang="ts">
    import {Card, Content, Header, Image} from "svelte-fomantic-ui";

    import type { Sentant } from './reality2_node.js';

    export let sentant: Sentant = {name: "", id: ""};
    
</script>
<!----------------------------------------------------------------------------------------------------->

<Card ui>
    <Image ui large src="/images/sentant.png"/>
    <Content>
        <Header>Name : {sentant.name}</Header>
    </Content>
    <Content extra>
        <p>{sentant.id}</p>
    </Content>
</Card>

