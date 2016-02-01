'''
leetcode #330: patching arrays: https://leetcode.com/problems/patching-array/
'''

class Solution(object):

  def minPatches(self, nums, n):
    """
    :type nums: List[int]
    :type n: int
    :rtype: int
    """
    n_range = [x for x in range(1,n+1)]
    # Generate the power set of nums
    powerSet = self.generatePowerSet(nums)
    print(powerSet)
    # Extract all possible sums
    sums = {sum(ps) : ps for ps in powerSet} 
    # Extract the missing number ({range} \ {sums.keys()})
    missing = [x for x in n_range if x not in sums.keys()]
    # Compute the list of differences foreach missing number
    # Reduce that list, then parse it to compute patches. 

  def generatePowerSet(self, ens):
    '''
    @args ens: a finite set of numbers
    @return powerSet: the set of all sets in ens
    '''

    number = 2 ** len(ens)
    power_set = []
    for n in range(number):
      a_set = []
      binary = self.toBinary(n)
      #print(binary)
      for b in range(len(binary)):
        a_set.append(ens[b] * binary[b])

      #print(a_set)
      power_set.append(a_set)

    #print(power_set[1::])
    return power_set[1::]

  def toBinary(self, number):
    '''
    @args number: an integer in base 10
    @return binary: an integer in base 2
    '''
    binary = []

    while (number > 0):
      if (number % 2 == 0):
        binary.append(0)
      else:
        binary.append(1)

      number = number / 2

    return binary[::-1]


ens = [1,2,3,4]

sol = Solution()
sol.minPatches(ens, 5)
