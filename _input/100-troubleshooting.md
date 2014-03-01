
# Troubleshooting

## Dependencies

Cause: after running `cabal install` or a similar command you see something similar to this:

```
Resolving dependencies...
cabal: Could not resolve dependencies:
trying: df-one-0.1
rejecting: digestive-functors-heist-0.8.4.1, 0.8.4.0, 0.8.3.1, 0.8.3.0,
0.8.1.0, 0.8.0.0 (conflict: df-one => digestive-functors-heist==0.7.0.0)
trying: digestive-functors-heist-0.7.0.0
rejecting: digestive-functors-0.7.0.0 (conflict: digestive-functors-heist =>
digestive-functors>=0.6.1 && <0.7)
rejecting: digestive-functors-0.6.2.0, 0.6.1.1, 0.6.1.0, 0.6.0.1, 0.6.0.0,
0.5.0.4, 0.5.0.3, 0.5.0.2, 0.5.0.1, 0.5.0.0, 0.4.1.2, 0.4.1.1, 0.4.1.0,
0.4.0.0, 0.3.2.1, 0.3.1.0, 0.3.0.2, 0.3.0.1, 0.3.0.0, 0.2.1.0, 0.2.0.1,
0.2.0.0, 0.1.0.2, 0.1.0.1, 0.1.0.0, 0.0.2.1, 0.0.2.0, 0.0.1 (conflict: df-one
=> digestive-functors>=0.7.0.0 && <0.8.0.0)
```

### How to Fix

This means your dependencies are mismatched and cabal can't find a solution. The way to resolve this is to look into the `.cabal` files of the relevant projects and choose versions that are compatible. For example, in the above example, the issue is that we are trying to install `digestive-functors-heist`, but we can't find one that satisfies our .cabal file.

The relevant part of our cabal file (in this example) looks like this under `Build-depends`:

```Haskell
digestive-functors        >= 0.7.0.0 && <0.8.0.0,
digestive-functors-heist  == 0.7.0.0
```

and the error tells us

```
rejecting: digestive-functors-0.7.0.0 (conflict: digestive-functors-heist =>
digestive-functors>=0.6.1 && <0.7)
```

What the error is saying here is that cabal is `rejecting digestive-functors` version `0.7.0.0` as a viable dependency because it `conflicts` with `digestive-functors-heist`'s requirements. It then continures to tell us that the relevant requirements for `digestive-functors-heist` include `digestive-functors` versions greater than or equal to `0.6.1` and less than `0.7`.

At this point, we know that since our cabal file says to install a `digestive-functors` version that is `>= 0.7.0.0` and that `digestive-functors-heist` depends on `digestive-functors` being at a version that is less than `0.7.0.0` we have a conflict in the version of `digestive-functors`. The solution, luckily for us, is simple. Since `digestive-functors-heist` needs a specific version range, we just put that in our .cabal file, which now looks like this:

```Haskell
digestive-functors        >=0.6.1 && <0.7,
digestive-functors-heist  == 0.7.0.0
```

The version that `digestive-functors-heist` needs will now be installed and the dependency error will go away (unless somthing else in the .cabal file also has errors).