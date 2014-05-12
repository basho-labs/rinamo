# Notes on DynamoDB

## API Reference:
* [API Reference](http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/Welcome.html)
  * [Create Table](http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_CreateTable.html)
  * [Get Item](http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_GetItem.html)
  * [Put Item](http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_PutItem.html)
  * [Query](http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_Query.html)


Code Flow, roughly:
rinamo_middleware_* -> rinamo_handler_root -> rinamo_api -> rinamo_codec -> [rinamo_tables|rinamo_items|indexing] -> rinamo_api -> rinamo_handler_root -> rinamo_middleware_*
