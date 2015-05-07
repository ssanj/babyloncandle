---
title: Calculated Risks have a Critical Mass
author: sanjiv sahayam
description: The cumulative effects of technical debt are unforeseeable.
tags: ideas, work
---

We all need to work very fast these days. Bugs in production are costing us money on an hourly basis. We may have a gaping security hole. We don't have the capacity to service all our current clients. We have to solve these problems ASAP. Or do we?

Technical debt is the main way these unplanned, extremely "urgent" features, changes or fixes get into production. We over estimate our capabilities constantly. We think we are taking "calculated" risks. In reality we are not doing any calculations. Our understanding of the current workings of the system is usually off. Way off. We have forgotten about the fix Joe did last week. Or the new configuration that is in place for special customers. We need to be [five whysing](http://www.isixsigma.com/tools-templates/cause-effect/determine-root-cause-5-whys) ourselves about "Why does this happen?".

You can only continue to incur technical debt for so long. If you keep incurring technical debt on a regular basis, there will come a tipping point where your system will become massively unstable. It will become exponentially harder to fix bugs and add new features. Developers will "give up" on the code base because they don't want to work on a foundation of hacks. Quality will slip. Your product will fail. Your company will go out of business.

All that sounds a bit dramatic. If you keep accumulating technical debt, your system will be foreclosed. It's not "if" but "when".

So how can you work with technical debt and still have a stable system? How can you fix critical issues and not piss off all your developers and operations people?  Here are some ideas:


# Investigate your risks

Actually spend a good chunk of time thinking about how the system works. Work in a pair if possible and look through the source code, configuration and architecture. Run your risk assessment past members of the team from different technical and non-technical areas. No one person should be making the risk call - specially not a non-technical person. If the risk is too great, think of alternative ways of solving your problem. Could you get an extension from the client? Can we bare with the issues we have now? Can we do nothing? What will it cost us?

# Have a repayment plan

Make a plan to pay off the technical debt after a certain release, feature/fix or by a certain date. Have an upper limit on the amount of debt you can accrue. Do not incur more debt after this limit has been reached. Sacrifice new features for stability through repayments.


# Make the smallest change possible

If you do decide to take on some technical debt, try to make it as small as possible. This is not the time to change the core architecture or make other sweeping changes. Make the change as small as possible. Make sure all your unit and integration tests pass. If they don't, make the smallest change possible to get them to pass or take them out of the build.

 > It's more important to have a suit of working tests that you can rely on over just ignoring failing tests. If you start ignoring test failures you will never have full confidence in your test suite as some of them are known to fail.

# Document your debts

Document the reasons for incurring the debt directly in the code that is affected by it. This can be done using comments or TODOs that you can come back to later. After you incur technical debt, add it to a debt register of some kind. This could be a wiki page, card wall or even a bunch of red postits on a wall or whiteboard. This will be a constant reminder of the debt you have already incurred. It also will help you to pay back this debt ASAP and prevent you from taking unnecessary new debt. As the wiki page or the wall grows, you know you are in trouble.

# Pay off your debts

Completely rewrite any code hacks documented in your debt registry. Reintroduce all excluded tests. Make them pass. Add more tests. Refactor. Win. :)





