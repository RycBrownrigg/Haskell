
{-

Overview

We will build a library of functions to handle storing ownership of different types of assets by different owners.

In order to have more readable code, we start by providing type synonyms for asset identifiers (which will just correspond to a string), and an asset store, which is just a list of pairs of (i) asset identifier; and (ii) count of that asset:

    type AssetId = String
    type AssetStore = [(AssetId, Integer)]

For example, the asset store [("GoldToken", 7), ("SilverToken", 4)] represents the situation in which we have 7 gold tokens and 4 silver tokens. Any unlisted tokens are taken to be absent i.e. in this last example, the store contains 0 copper tokens.

In the rest of the assignment, we will assume that an asset store will never include multiple instances of the same token identifier i.e. we will never see [("GoldToken", 7), ("SilverToken", 4), ("GoldToken", 1)], since it con- tains two counts for GoldToken.

================================================================

ANSWER:

In building the library of functions for handling the storage of asset ownership, I followed the steps outlined above. Each function will be fully commented and explained.

First, I will define the type synonyms for better readability and code organization:

-}

type AssetId = String -- The AssetId is a string
type AssetStore = [(AssetId, Integer)] -- The AssetStore is a list of tuples, where the first element is the AssetId and the second element is the quantity of that asset
type Address = Integer -- The Address is an integer
type AssetOwnershipStore = [(Address, AssetStore)] -- The AssetOwnershipStore is a list of tuples, where the first element is the Address and the second element is the AssetStore

{-

================================================================

QUESTION 1: Using these type synonyms, define a function which, given an asset store and an asset identifier, returns how many of that token are available in the store:

    countAsset :: AssetStore -> AssetId -> Integer

================================================================

ANSWER 1:

Counting assets in a store

Define a function that, given an asset store and an asset identifier, returns the count of that asset in the store. If the asset is not present in the store, it should return 0.

-}

-- Given an asset store and an asset identifier, return the count of that asset.

countAsset :: AssetStore -> AssetId -> Integer -- The function countAsset receives an AssetStore and an AssetId as input and returns an Integer
countAsset [] _ = 0 -- If the AssetStore is empty, return 0
countAsset ((aid, count):xs) assetId -- If the AssetStore is not empty, check if the asset identifier matches the current asset identifier
    | aid == assetId = count -- If the asset identifier matches, return the count of that asset
    | otherwise      = countAsset xs assetId -- If the asset identifier does not match, recursively call countAsset with the rest of the AssetStore

{-
Explanation:

- The function 'countAsset' takes an 'AssetStore' and an 'AssetId' as inputs.
- It recursively checks each pair in the list.
- If it finds a pair with the matching 'AssetId', it returns the count.
- If it doesn't find a match, it continues checking the rest of the list.
- If the list is exhausted without finding a match, it returns 0.

================================================================

QUESTION 2: Define a function updateStore to update an asset store by adding a given value of assets with a particular identifier. For example, updateStore store ("GoldToken", 3) will add 3 gold tokens to the store. If the store was [("GoldToken", 7), ("SilverToken", 4)], the result would be [("GoldToken", 10), ("SilverToken", 4)], whereas if the store was [("CopperToken", 7), ("SilverToken", 4)], the result would be [("GoldToken", 3), ("CopperToken", 7), ("SilverToken", 4)]:
    updateAsset :: AssetStore -> (AssetId, Integer) -> AssetStore

================================================================

ANSWER 2:

Updating the asset store

Develop a function to update an asset store by adding a given number of assets. If the asset already exists in the store, we increment its count. If it doesn't exist, we add it to the store.

-}

-- Update the asset store by adding a given number of assets with a specific identifier.

updateAsset :: AssetStore -> (AssetId, Integer) -> AssetStore -- The function updateAsset receives an AssetStore and a tuple with an AssetId and an Integer as input and returns an AssetStore
updateAsset [] (assetId, count) = [(assetId, count)] -- If the AssetStore is empty, add the asset with the given count to the store
updateAsset ((aid, c):xs) (assetId, count) -- If the AssetStore is not empty, check if the asset identifier matches the current asset identifier
    | aid == assetId = (aid, c + count) : xs -- If the asset identifier matches, update the count of that asset and return the updated store
    | otherwise      = (aid, c) : updateAsset xs (assetId, count) -- If the asset identifier does not match, recursively call updateAsset with the rest of the AssetStore

{-

Explanation:

- The function 'updateAsset' takes an 'AssetStore' and a tuple of 'AssetId' and 'Integer'.
- It checks if the asset store is empty. If so, it creates a new entry with the given asset and count.
- It recursively checks each pair in the list:
    - If it finds a pair with the matching AssetId, it updates the count.
    - If it doesn't find a match, it continues checking the rest of the list and reconstructs the list with the updated asset.
    - If the asset is not found in the store, it adds a new entry for the asset with the given count.

================================================================

We now add the notion of the owner of a particular asset store. User addresses will be represented using the type synonym Address, corresponding to an integer. The type 'AssetOwnershipStore' corresponds to pairs of (i) address; and (ii) asset store owned by that address:
    
        type Address = Integer
        type AssetOwnershipStore = [(Address, AssetStore)]

For example, the asset ownership store [(1, [("GoldToken", 10), ("SilverToken", 4)]), (4, [("GoldToken", 1)])] has user at address 1 having 10 gold and 4 silver tokens, whilst the user at address 4 having 1 gold token. All other users are assumed not to own any tokens.

Similar to the case of 'AssetStore', in the rest of the assignment we will assume that users do not appear more than once in an asset ownership store.

QUESTION 3: Using these type synonyms, define a function which, given an asset ownership store, an address, and an asset identifier, returns how many of that token are owned by that user:

    countAssetByAddress :: AssetOwnershipStore -> (Address, AssetId) -> Integer

================================================================

ANSWER 3:

Counting assets by address

Define a function that, given an asset ownership store, an address, and an asset identifier, returns the count of that asset owned by that user.

-}

-- Given an asset ownership store, an address, and an asset identifier, return the count of that asset owned by that user.

countAssetByAddress :: AssetOwnershipStore -> (Address, AssetId) -> Integer -- The function countAssetByAddress receives an AssetOwnershipStore and a tuple with an Address and an AssetId as input and returns an Integer. 
countAssetByAddress [] _ = 0 -- If the AssetOwnershipStore is empty, return 0. 
countAssetByAddress ((addr, store):xs) (address, assetId) -- If the AssetOwnershipStore is not empty, check if the address matches the current address. 
    | addr == address = countAsset store assetId -- If the address matches, call countAsset with the asset store and asset identifier. 
    | otherwise       = countAssetByAddress xs (address, assetId) -- If the address does not match, recursively call countAssetByAddress with the rest of the AssetOwnershipStore. 

{-

Explanation:

- The function 'countAssetByAddress' takes an 'AssetOwnershipStore', an 'Address', and an 'AssetId'.
- It recursively checks each pair in the list:
    - If it finds a pair with the matching 'Address', it calls 'countAsset' to get the count of the specified asset in the user's store.
    - If it doesn't find a match, it continues checking the rest of the list.
- If the list is exhausted without finding a match, it returns 0. 
- If the user is not found in the store, it returns 0.

================================================================

QUESTION 4: Define a function updateAssetByAddress to update an asset ownership store by adding a given number of assets with a particular identifier to a given user’s store:

    updateAssetByAddress :: 
       AssetOwnershipStore ->
        (Address, AssetId, Integer) -> AssetOwnershipStore

================================================================

Updating assets by address

Define a function to update an asset ownership store by adding a given number of assets with a particular identifier to a given user's store. If the user doesn't exist, we add a new entry for the user.

-}

-- Update an asset ownership store by adding a given number of assets with a specific identifier to a user's store.

updateAssetByAddress :: AssetOwnershipStore -> (Address, AssetId, Integer) -> AssetOwnershipStore -- The function updateAssetByAddress receives an AssetOwnershipStore and a tuple with an Address, an AssetId, and an Integer as input and returns an AssetOwnershipStore.
updateAssetByAddress [] (address, assetId, count) = [(address, [(assetId, count)])] -- If the AssetOwnershipStore is empty, add a new entry for the user with the asset and count. 
updateAssetByAddress ((addr, store):xs) (address, assetId, count) -- If the AssetOwnershipStore is not empty, check if the address matches the current address. 
    | addr == address = (addr, updateAsset store (assetId, count)) : xs -- If the address matches, update the user's store with the new asset and count. 
    | otherwise       = (addr, store) : updateAssetByAddress xs (address, assetId, count) -- If the address does not match, recursively call updateAssetByAddress with the rest of the AssetOwnershipStore.     

{-

Explanation:

- The function 'updateAssetByAddress' takes an 'AssetOwnershipStore' and a tuple of 'Address', 'AssetId', and 'Integer'.
- It checks if the asset ownership store is empty. If so, it creates a new entry for the user with the given asset and count.
- It recursively checks each pair in the list:
    - If it finds a pair with the matching Address, it updates the user's asset store using the updateAsset function.
    - If it doesn't find a match, it continues checking the rest of the list and reconstructs the list with the updated user's store.
    - If the user is not found in the store, it adds a new entry for the user with the given asset and count.

These functions provide the basic operations for handling asset ownership in a Haskell program.
-}

