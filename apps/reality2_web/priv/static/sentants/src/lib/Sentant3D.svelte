<script lang="ts">
    import { T } from '@threlte/core'

    export let position = [0, 0, 0];
</script>

<T.Mesh>
    <T.SphereGeometry args={[3, 32, 16]} />
    <T.MeshBasicMaterial color="blue" />
    position={position}
</T.Mesh>