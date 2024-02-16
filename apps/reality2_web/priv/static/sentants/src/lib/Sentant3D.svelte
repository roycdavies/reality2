<script lang="ts">
    import { T } from '@threlte/core'

    export let position = [0, 0, 0];
</script>

<T.Mesh position={position}>
    <T.SphereGeometry args={[1, 32, 16]} />
    <T.MeshBasicMaterial color="blue" />
</T.Mesh>