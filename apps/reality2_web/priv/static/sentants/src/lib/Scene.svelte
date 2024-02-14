<script lang="ts">
    import { T } from '@threlte/core'

    import Sentant3D from './Sentant3D.svelte';
</script>
<T.PerspectiveCamera
    position={[10, 10, 10]}
    on:create={({ ref }) => {
        ref.lookAt(0, 0, 0)
    }}
/>

<Sentant3D />
<Sentant3D position={[4,4,4]}/>