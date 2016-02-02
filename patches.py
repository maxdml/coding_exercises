'''
leetcode #330: patching arrays: https://leetcode.com/problems/patching-array/
'''

class Solution(object):

  """
  minPatches attempt 1: "brute force" solution, trying to discover
  all missing patches after computing all the possible sums
  """
  def minPatches(self, nums, n):
    """
    :type nums: List[int]
    :type n: int
    :rtype: int
    """

    n_range = [x for x in range(1,n+1)]
    print("Getting patches for range " +  str(n_range) + " and set " + str(nums))

    # Generate the power set of nums
    powerSet = self.generatePowerSet(nums)
    print("powerset of nums is : " + str(powerSet))

    # Extract all possible sums
    sums = {sum(ps) : ps for ps in powerSet}
    print("Present sums are: " + str(sums.keys()))

    # Extract the missing number ({range} \ {sums.keys()})
    missing = [x for x in n_range if x not in sums.keys()]
    print("Missing computable numbers are: " + str(missing))

    # Compute the list of differences foreach missing number
    missing_diffs = set()
    for m in missing:
      missing_diffs.update([min({abs(x - m) for x in sums.keys()})])
    print("Missing differences are: " + str(missing_diffs))

    # For each missing difference, get the set of missing number it can complete. Patches is the
    # smallest union of the missing differences which can complete all missing numbers
    A = {}
    for d in missing_diffs:
      A[d] = []
      for m in missing:
        if (m - d in sums.keys()):
          A[d].append(m)

    print(A)

  """
  minPatches attempt 2: Greedy algorithm discovering patches while parsing 
  the provided set
  def minPatches(self, nums, n):
  """

  def generatePowerSet(self, ens):
    '''
    @args ens: a finite set of numbers
    @return powerSet: the set of all sets in ens
    '''

    number = 2 ** len(ens)
    power_set = []
    for n in range(0, number):
      a_set = set()
      binary = self.toBinary(n)
      zeros = [0 for x in range(len(ens) - len(binary))]
      binary = zeros + binary
      #print(binary)
      for b in range(len(binary)):
        a_set.update([ens[b] * binary[b]])

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


#ens = [1,2,3]
ens = [1,5,10]

sol = Solution()
#print(sol.minPatches(ens, 8))
print(sol.minPatches(ens, 20))
