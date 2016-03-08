module Capital.Demo.Service where


createDemo :: Demo -> DemoService ()
createDemo = applyCommand . AddDemo

updateDemo :: DemoId -> Demo -> DemoService ()
updateDemo did d = applyCommand . UpdateDemo did

deleteDemo :: DemoId -> DemoService ()
deleteDemo = applyCommand . DeleteDemo

