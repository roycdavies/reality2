<script lang="ts">
    import { T } from '@threlte/core'

//     import {
//     Canvas,
//     DirectionalLight,
//     HemisphereLight,
//     Mesh,
//     OrbitControls,
//     PerspectiveCamera
//   } from "threlte";

    import Sentant3D from './Sentant3D.svelte';
</script>

<T.DirectionalLight shadow color={'white'} position={{ x: -15, y: 45, z: 20 }} />
<T.HemisphereLight skyColor={'white'} groundColor={'#ac844c'} intensity={0.4} />

<T.PerspectiveCamera
    position={[50, 50, 50]}
    on:create={({ ref }) => {
        ref.lookAt(0, 0, 0)
    }}
/>


<Sentant3D />
<Sentant3D position={[2,0,0]}/>